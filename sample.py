# %% Import packages
import numpy as np
import pandas as pd
import random

# %% Define sampling functions
def simple_random_sample(gdf, n):
    """
    Returns a simple random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    return gdf.sample(n=n)

def stratified_random_sample(gdf, n):
    """
    Returns a stratified random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    
    races = ["white_non_hispanic",
        "black_non_hispanic",
        "asian_non_hispanic",
        "other_non_hispanic",
        "hispanic"]
    
    # Make a copy of the dataframe
    gdf_copy = gdf.copy()

    # Calculate total populations for each race and for the county
    total_pops = gdf_copy[["total_population"] + races].sum(axis=0)

    # # Calculate the proportions of races within the county
    # for race in races:
    #     col_name = race + " in County"
    #     gdf_copy[col_name] = gdf_copy[race]/total_pops["Total Population"]

    # # Calculate the proportions of races within each tract
    # for race in races:
    #     col_name = race + " in Tract"
    #     gdf_copy[col_name] = gdf_copy[race]/gdf_copy["Total Population"]

    # # Calculate the tract race percentiles using tract-level proportions
    # for race in races:
    #     col_name = race + " Pct (Tract)"
    #     gdf_copy[col_name] = gdf_copy[race + " in Tract"].rank(pct=True)

    # # Label each tract with the race with the highest percentile

    # Convert table to long format
    gdf_copy = pd.melt(gdf_copy,
        id_vars=set(gdf_copy.columns)-set(races),
        value_vars=races,
        var_name="race",
        value_name="population")
    
    # Calculate the proportions of races within the county
    gdf_copy["proportion_in_county"] = (gdf_copy["population"]/total_pops["total_population"]).replace([np.inf, -np.inf, np.nan], 0)
   
    # Calculate the proportions of races within each tract
    gdf_copy["proportion_in_tract"] = (gdf_copy["population"]/gdf_copy["total_population"]).replace([np.inf, -np.inf, np.nan], 0)
  
    # Calculate the tract race percentiles using tract-level proportions
    gdf_copy["percentile"] = gdf_copy.groupby("race")["proportion_in_tract"].rank(pct=True)

    # Label each tract with the race with the highest percentile
    gdf_copy = gdf_copy.loc[gdf_copy.groupby("GEOID")["percentile"].idxmax()]

    # Check if there are any duplicate IDs or null values

    # Check that there are enough tracts for every race group
        # Count the tracts by race category
        # Compare to total county population by race

def systematic_random_sample(gdf, n):
    """
    Returns a systematic random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    
    # Make a copy of the dataframe
    gdf_copy = gdf.copy()

    # Rank tracts by the north-south coordinate of their centroids
    gdf_copy["rank"] = gdf_copy.centroid.y.rank(method="first", ascending=False)
    gdf_copy["rank"] = gdf_copy["rank"].astype(int)

    # Choose a random number between 1 and the number of tracts
    start = random.randint(1, len(gdf_copy))
    step = len(gdf_copy) / n

    # Select tracts spaced by step, starting at start
    idxs = [(start + int(i*step)) % len(gdf_copy)
        for i in range(n)]
    sample = gdf_copy.loc[gdf_copy["rank"].isin(idxs)]

    # Return the original tracts, not the ranks
    sample = gdf.loc[sample.index]
    return sample

def cluster_random_sample(gdf, n):
    """
    Returns a cluster random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    
    # Create a new column with centroids of each polygon
    gdf_cents = gdf.copy()
    gdf_cents["centroid"] = gdf_cents.centroid

    # Select a random tract from gdf_cents
    focus = gdf_cents.sample(n=1)

    # Find the distance between the centroid of the focus tract and all other tracts
    gdf_cents["dist"] = gdf_cents.centroid.distance(focus.centroid.iloc[0])

    # Sort by distance and select the n closest tracts
    sample = gdf_cents.sort_values(by="dist").head(n)

    # Return the original tracts, not the centroids
    sample = gdf.loc[sample.index]
    return sample
# %%