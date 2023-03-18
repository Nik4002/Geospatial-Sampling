# %% Import libraries
import pandas as pd
import geopandas as gpd
# from census import write_county_data, VARS
# %% Define functions
def get_data(county, state):
    """
    Gets data for a given county and state from Data folder 
    [ADD LATER: If it doesn't exist, then calls census.write_county_data() to create it]

    Parameters:
        county (str): County name (e.g. "Bexar")
        state (str): State name (e.g. "TX")
    
    Returns:
        data (DataFrame): Data for given county and state
    """
    # try:
    #     data = gpd.read_parquet("Data/" + county + "_County_" + state + ".parquet")
    # except FileNotFoundError:
    #     write_county_data(county, state, VARS)
    #     data = gpd.read_parqut("Data/" + county + "_County_" + state + ".parquet")
    # return data
    return gpd.read_parquet("Data/" + county + "_County_" + state + ".parquet")

def clean_data(gdf):
    """
    Cleans given GeoDataFrame (renames columns, drops unnecessary columns, etc.)
    """
    # Rename columns
    gdf = gdf.rename(columns={
        "B01003_001E": "total_population",
        "B03002_001E": "total_population_race",
        "B03002_002E": "total_non_hispanic",
        "B03002_003E": "white_non_hispanic",
        "B03002_004E": "black_non_hispanic",
        "B03002_005E": "native_non_hispanic",
        "B03002_006E": "asian_non_hispanic",
        "B03002_007E": "pacific_non_hispanic",
        "B03002_008E": "other_non_hispanic",
        "B03002_009E": "two_or_more_non_hispanic",
        "B03002_012E": "hispanic",
        "B19013_001E": "median_income",
        # "B19001B_001E": "Total (Income)",
        # "B19001B_002E": "Less than $10,000",
        # "B19001B_003E": "$10,000 to $14,999",
        # "B19001B_004E": "$15,000 to $19,999",
        # "B19001B_005E": "$20,000 to $24,999",
        # "B19001B_006E": "$25,000 to $29,999",
        # "B19001B_007E": "$30,000 to $34,999",
        # "B19001B_008E": "$35,000 to $39,999",
        # "B19001B_009E": "$40,000 to $44,999",
        # "B19001B_010E": "$45,000 to $49,999",
        # "B19001B_011E": "$50,000 to $59,999",
        # "B19001B_012E": "$60,000 to $74,999",
        # "B19001B_013E": "$75,000 to $99,999",
        # "B19001B_014E": "$100,000 to $124,999",
        # "B19001B_015E": "$125,000 to $149,999",
        # "B19001B_016E": "$150,000 to $199,999",
        # "B19001B_017E": "$200,000 or More"
        })
    gdf = gdf.drop(columns=["state", "county"])

    # Specify data types for each column

    # Add a column with the tract's centroid
    gdf["centroid"] = gdf.centroid

    # Combine native, pacific , other, and two or more into "other"
    gdf["other_non_hispanic"] = gdf["other_non_hispanic"] + gdf["native_non_hispanic"] + gdf["pacific_non_hispanic"] + gdf["two_or_more_non_hispanic"]
    gdf = gdf.drop(columns=["native_non_hispanic", "pacific_non_hispanic", "two_or_more_non_hispanic"])

    check_data_validity(gdf)

    return gdf

def check_data_validity(gdf):
    """
    Checks that the data is valid (e.g. no duplicate values, no missing values,
    and all columns sum to corresponding totals)
    """
    # Assert that there are no duplicate values in the tract column
    assert not gdf["tract"].duplicated().any(), "Duplicate values in tract column"

    # Assert that there are no missing values
    assert not gdf.isnull().values.any(), "Missing values"

    # Check that race columns sum to total population
    non_hispanic = gdf[["white_non_hispanic", "black_non_hispanic", "asian_non_hispanic", "other_non_hispanic"]]
    assert non_hispanic.sum(axis=1).equals(gdf["total_non_hispanic"])
    assert gdf[["total_non_hispanic", "hispanic"]].sum(axis=1).equals(gdf["total_population_race"])
    assert gdf["total_population_race"].equals(gdf["total_population"])

# %%
