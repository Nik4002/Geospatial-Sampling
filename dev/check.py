# %% Import packages
import numpy as np
import pandas as pd
import geopandas as gpd
from sample import simple_random_sample, stratified_random_sample, systematic_random_sample, cluster_random_sample

# %% Define a wrapper function to repeatedly use a sampling method
def sample(method, gdf, sample_size, n, seed=1):
    """
    Returns the averages of each of n samples of size sample_size from gdf
    using the given sampling method
    """
    
    map = {"simple": simple_random_sample,
        "stratified": stratified_random_sample,
        "systematic": systematic_random_sample,
        "cluster": cluster_random_sample}
    sampling_function = map[method]

    # Create a list to store the average income for each sample
    means = []

    for i in range(n):
        # Sample from the dataframe
        sample = sampling_function(gdf, sample_size, seed)

        # Calculate the mean income for the sample
        mean = sample["median_income"].mean()

        # Add the mean to the list
        means.append(mean)
    
    return means

def describe(means):
    return pd.Series(means).describe()

def plot(means):
    pd.Series(means).plot.hist()
# %%
