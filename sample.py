# %% Import packages
import numpy as np
import pandas as pd
import geopandas as gpd
import random
import scipy.stats as stats

# %% Define function to get n stratified tracts
def subset(gdf, n, seed=1):
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

    # Make a geometry side table and a shortened table to be pivoted
    geoms = gdf[["tract", "geometry", "centroid", "GEOID", "NAME"]]
    to_drop = ["geometry",
        "total_population_race",
        "centroid",
        "total_non_hispanic",
        "GEOID",
        "NAME"]
    gdf_copy = pd.DataFrame(gdf_copy.drop(columns=to_drop))

    # Calculate total populations for each race and for the county
    total_pops = gdf_copy[["total_population"] + races].sum(axis=0)

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
    gdf_copy = gdf_copy.loc[gdf_copy.groupby("tract")["percentile"].idxmax()]
    gdf_copy = gdf_copy.drop(columns=["proportion_in_county", "proportion_in_tract", "percentile"])

    # Check if parameters are valid
    assert gdf_copy.shape[0] >= n, 'Sample size must be less than or equal to the number of rows in the dataframe'
    # assert len(set(gdf_copy["race"].unique())-set(total_pops["race"].unique())) == 0, 'The race values must be the same in both dataframes'

    # Turn total population into a dataframe
    total_pops = total_pops.to_frame().reset_index().rename(columns={"index": "race", 0: "total_population"})

    # Draw stratified sample
    gdf_copy = pd.merge(left = gdf_copy, right = total_pops, on = "race", how = 'left')
    sampling_frac = n / gdf_copy.shape[0]
    sample_df = gdf_copy.groupby("race").apply(lambda x: x.sample(frac = sampling_frac, weights="population", random_state=seed)).reset_index(drop = True)
    assert sample_df.shape[0] == n, 'Sample size is not equal to the desired sample size'

    # Check if sample is representative of population
    a = pd.concat([sample_df.groupby(by=["race"])["population"].agg('sum') / sample_df["population"].sum()],axis=1).reset_index()
    b = pd.concat([total_pops.groupby(by=["race"])["total_population"].agg('sum') / total_pops["total_population"].sum()],axis=1).reset_index()
    c = a.merge(b, on = "race", how = 'inner').rename(columns = {"total_population": "population_proportion", "population": "sample_proportion"})
    kl_divergence = round(stats.entropy(pk=c["sample_proportion"], qk=c["population_proportion"]),4)
    # if kl_divergence > 0.05:
    #     print(f'Sample may not be sufficiently representative of population. Consider changing random seed or increasing n. Kullback-Leibler divergence is {kl_divergence}.')
    #     print(c)
    # else:
    #     print(f'Sample is representative of population. Kullback-Leibler divergence is {kl_divergence}.')
    #     print(c)

    # Merge sample with geometry side table
    sample_df = sample_df.drop(columns="total_population_y").merge(geoms, on = "tract", how = 'left').rename(columns={"total_population_x": "total_population"})
    
    # Convert to geodataframe
    sample_df = gpd.GeoDataFrame(sample_df, geometry="geometry", crs=gdf.crs)

    return sample_df

# %% Define sampling functions
def simple_random_sample(gdf, n, seed=1):
    """
    Returns a simple random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample
        seed (int): Random seed

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    gdf = subset(gdf, 100, seed=seed)
    return gdf.sample(n=n)

def stratified_random_sample(gdf, n, seed=1):
    """
    Returns a stratified random sample of n rows from a geodataframe
    
    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample
        seed (int): Random seed

    Returns: (dataframe) n rows from gdf    
    """
    gdf = subset(gdf, 100, seed=seed)
    # print(gdf)
    return gdf.groupby("race").sample(n//5)

def systematic_random_sample(gdf, n, seed=1):
    """
    Returns a systematic random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample
        seed (int): Random seed

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    
    gdf = subset(gdf, 100, seed=seed)

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

def cluster_random_sample(gdf, n, seed=1):
    """
    Returns a cluster random sample of n rows from a geodataframe

    Parameters:
        gdf (dataframe): Dataframe to sample
        n (int): Number of rows to sample
        seed (int): Random seed

    Returns: (dataframe) n rows from gdf
    """
    assert 0 < n <= len(gdf), "n must be between 0 and the length of the dataframe"
    
    gdf = subset(gdf, 100, seed=seed)

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