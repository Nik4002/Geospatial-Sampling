# %% Import packages
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
    pass

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
