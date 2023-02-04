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
    #     data = pd.read_csv("Data/" + county + "_County_" + state + ".csv")
    # except FileNotFoundError:
    #     write_county_data(county, state, VARS)
    #     data = pd.read_csv("Data/" + county + "_County_" + state + ".csv")
    # return data
    return gpd.read_file("Data/" + county + "_County_" + state + ".shp")

def clean_data(gdf):
    """
    Cleans given GeoDataFrame (renames columns and drops unnecessary columns)
    """
    gdf = gdf.rename(columns={"B03002_001": "Total",
        "B03002_002": "Total non-Hispanic",
        "B03002_003": "White non-Hispanic",
        "B03002_004": "Black non-Hispanic",
        "B03002_005": "Native non-Hispanic",
        "B03002_006": "Asian non-Hispanic",
        "B03002_007": "Pacific non-Hispanic",
        "B03002_008": "Other non-Hispanic",
        "B03002_009": "Two or More non-Hispanic",
        "B03002_012": "Hispanic",
        "B19001B__1": "Total",
        "B19001B__2": "Less than $10,000",
        "B19001B__3": "$10,000 to $14,999",
        "B19001B__4": "$15,000 to $19,999",
        "B19001B__5": "$20,000 to $24,999",
        "B19001B__6": "$25,000 to $29,999",
        "B19001B__7": "$30,000 to $34,999",
        "B19001B__8": "$35,000 to $39,999",
        "B19001B__9": "$40,000 to $44,999",
        "B19001B_10": "$45,000 to $49,999",
        "B19001B_11": "$50,000 to $59,999",
        "B19001B_12": "$60,000 to $74,999",
        "B19001B_13": "$75,000 to $99,999",
        "B19001B_14": "$100,000 to $124,999",
        "B19001B_15": "$125,000 to $149,999",
        "B19001B_16": "$150,000 to $199,999",
        "B19001B_17": "$200,000 or More"})
    gdf = gdf.drop(columns=["state", "county"])
    return gdf
# %%
