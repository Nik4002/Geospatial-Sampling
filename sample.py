# %% Import packages
import pandas as pd
import random

# %% Define sampling functions
def simple_random_sample(df, n):
    """
    Returns a random sample of n rows from a dataframe

    Parameters:
        df (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n randomly selected rows from df
    """
    return df.sample(n=n)

def stratified_random_sample(df, n):
    """
    Returns a stratified random sample of n rows from a dataframe

    Parameters:
        df (dataframe): Dataframe to sample
        n (int): Number of rows to sample

    Returns: (dataframe) n randomly selected rows from df
    """
    # # Get number of rows for each unique value in column
    # counts = df["column"].value_counts()
    # # Get number of rows to sample from each unique value in column
    # counts = counts.apply(lambda x: round(x * n / len(df)))
    # # Get random sample of each unique value in column
    # samples = df.groupby("column").apply(lambda x: x.sample(n=counts[x.name]))
    # # Return all samples in a single dataframe
    # return pd.concat(samples)