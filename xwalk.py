# %% Import packages
import cenpy as cen
import pandas as pd
import geopandas as gpd

from census import *
from data import *

# %% Functions
# Function that takes a xwalk and returns the relevant data
def get_xwalk_data(xwalk):
    # Use county_lookup to generate a FIPS table
    # Get the unique county FIPS codes from xwalk
    codes = {c[:5] for c in xwalk["tract_geoid"].to_list()}

    # Use FIPS codes for get_counties_data()
    data = get_counties_data(codes)

    # Merge data and xwalk using merge_xwalk()
    return merge_xwalk(data, xwalk)

def county_lookup():
    """
    Function that returns a cleaned-up FIPS county lookup table

    Parameters:
        none
    
    Returns:
        (DataFrame): FIPS lookup table
    """
    result = pd.read_csv("county_lookup.csv",
        dtype={"0": str, "1": str, "2": str, "3": str, "4": str})
    result = result.drop(columns=["Unnamed: 0", "4"], axis=1)
    result = result.rename(columns={"0": "state", "1": "state_fips", "2": "county_fips", "3": "county"})
    result["fips"] = result["state_fips"].str.zfill(2) + result["county_fips"].str.zfill(3)

    return result

def get_counties_data(fips):
    """
    Function that takes a set of county FIPS codes and pulls their data,
    returning one dataframe with all of the data

    Parameters:
        fips (iterable): Collection of county FIPS codes
    
    Returns:
        (GeoDataFrame): Collective tract data
    """
    # Create a fips table
    lookup = county_lookup()

    # Generate a list of county names using the fips table
    counties = lookup[lookup["fips"].isin(fips)]

    # Pull data for each county
    county_data = []
    for _, c in counties.iterrows():
        county_data.append(get_data(c["county"], c["state"]))

    # Use pd.concat([COUNTIES]) to merge the tables
    return pd.concat(county_data)

# Function that merges county data with xwalk
def merge_xwalk(data, xwalk):
    """
    Function that merges a xwalk with a dataset

    Parameters:
        xwalk (GeoDataFrame): Xwalk
        data (GeoDataFrame): Data
    
    Returns:
        (GeoDataFrame): Merged data
    """
    # Merge data and xwalk
    result = data.merge(xwalk, left_on="GEOID", right_on="tract_geoid", how="left")

    # Fix column labels
    result = result.rename(columns={"county_x": "county", "state_x": "state", "geometry_x": "geometry"})
    result = result.drop(columns=["geometry_y", "tract_geoid"])
    result = result.set_geometry("geometry")

    return result
# %%
