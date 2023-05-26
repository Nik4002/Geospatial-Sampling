# %% Import libraries
import pandas as pd
import geopandas as gpd
import cenpy as cen
from census import write_county_data, write_city_data, connect_acs, VARS
from sample import race_label
# %% Define functions
def split_place_name(place):
    """
    Splits a place name into city and state names

    Parameters:
        place (str): Place name (e.g. "San Antonio, TX")
    
    Returns:
        city (str): City name (e.g. "San Antonio")
        state (str): State name (e.g. "TX")
    """
    place_split = place.split(", ")
    city = place_split[0]
    state = place_split[1]
    return city, state

def state_abbrev(state):
    """
    Returns the state abbreviation given the state name

    Parameters:
        state (str): State name (e.g. "Texas")

    Returns:
        abbrev (str): State abbreviation (e.g. "TX")
    """
    abbr = pd.read_csv("State_Abbreviations.csv")
    return abbr.set_index("State").loc[state, "Abbr"]

def colloquial(key):
    """
    Returns the colloquial name of a city, given its full Census name
    (e.g. "Urban Honolulu CDP" -> "Honolulu", "Chicago city" -> "Chicago")

    Parameters:
        key (str): Census city name (e.g. "San Antonio city")
    
    Returns:
        city (str): Colloquial city name (e.g. "San Antonio")
    """
    # Handle special cases
    if key == "Urban Honolulu CDP":
        return "Honolulu"
    elif key == "Nashville-Davidson metropolitan government (balance)":
        return "Nashville"
    elif key == "Louisville/Jefferson County metro government (balance)":
        return "Louisville"
    elif key == "Lexington-Fayette urban county":
        return "Lexington"

    city = key

    # # Remove " city" from key
    # city = city.replace(" city", "")

    # # Remove " municipality" from key
    # city = city.replace(" municipality", "")

    # # Remove " town" from key
    # city = city.replace(" town", "")

    # # Remove " balance" from key
    # city = city.replace(" (balance)", "")

    # Split city into words
    city_split = city.split(" ")

    # Remove words that do not start with uppercase letters
    city_split = [word for word in city_split if word[0].isupper()]

    # Join words back together
    city = " ".join(city_split)

    return city

def place_table():
    """
    Returns a GeoDataFrame of places in the US
    """
    return cen.explorer.fips_table("place")

def place_type(city, state, places):
    """
    Returns the type of place (Incorporated Place, Census Designated Place, etc.)
    for the given city and state

    Parameters:
        city (str): City name (e.g. "San Antonio city")
        state (str): State name (e.g. "TX")
        places (GeoDataFrame): GeoDataFrame of places in the US

    Returns:
        (str): Type of place (e.g. "Incorporated Place")
    """
    matches = places[(places["PLACENAME"] == city) & (places["STATE"] == state)]
    types = matches["TYPE"].unique().tolist()
    if "Incorporated Place" in types:
        return "Incorporated Place"
    elif "Census Designated Place" in types:
        return "Census Designated Place"
    elif "County Subdivision" in types:
        return "County Subdivision"
    else:
        raise KeyError("Invalid input")

def add_city(key, city, state, acs=connect_acs()):
    """
    Adds a city to parquet file called "cities.parquet"

    Parameters:
        key (str): Census API name for city (e.g. "San Antonio city")
        city (str): Colloquial name (e.g. "San Antonio")
        state (str): State name (e.g. "TX")
    
    Returns:
        None
    """
    # Read data
    places = place_table()

    type = place_type(key, city, places)

    try:
        data = get_city_data(key, state, type, acs=acs, write=False)
    except:
        print(f"{city}, {state} data could not be pulled")
        return False
    
    # Add place name columns
    data["colloquial_name"] = city
    data["place_name"] = key
    data["state"] = state

    # Check if data is empty
    if data.empty:
        print(f"{city}, {state} produced an empty dataframe")
        return False
    
    # # Stratify data by race
    # races = ["white_non_hispanic",
    #     "black_non_hispanic",
    #     "asian_non_hispanic",
    #     "other_non_hispanic",
    #     "hispanic"]
    # data = race_label(data, races)

    # Randomly assign a label to each tract (just so we have a column to use in regionalization later)

    # Read cities.parquet
    try:
        cities = gpd.read_parquet("cities.parquet")
    except FileNotFoundError:
        cities = gpd.GeoDataFrame()

    # Create a copy of cities
    cities_copy = cities.copy()

    # Add city to cities.parquet using concat
    # cities = gpd.concat([cities, data])
    cities = gpd.GeoDataFrame(pd.concat([cities, data], ignore_index=True))

    # Remove duplicates by GEOID
    cities = cities.drop_duplicates(subset="GEOID")

    # Check if cities.parquet has changed
    if cities.equals(cities_copy):
        print(f"{city}, {state} has already been added to cities.parquet")
        return False

    # Write cities.parquet
    cities.to_parquet("cities.parquet")

    return True

# %%
def get_county_data(county, state):
    """
    Reads and cleans data for a given county and state
    """
    # Read data
    data = read_county_data(county, state)

    # Clean data
    data = clean_data(data)

    return data

def get_city_data(city, state, write=True):
    """
    Reads and cleans data for a given city and state
    """
    # Read data
    data = read_city_data(city, state, write=write)

    # Clean data
    data = clean_data(data)

    return data

def read_city_data(city, state, placetype, acs=connect_acs(), write=False):
    """
    Reads data for a given city and state from Data folder in parquet format

    Parameters:
        city (str): City name (e.g. "San Antonio city")
        state (str): State name (e.g. "TX")
        acs (census.ACS): ACS connection object
        write (bool): If True, then writes data to parquet file
    
    Returns:
        data (DataFrame): Data for given city and state
    """
    if write == True:
        try:
            data = gpd.read_parquet("Data/" + city.replace(" ", "_") + "_" + state + ".parquet")
        except FileNotFoundError:
            write_city_data(city, state, VARS)
            data = gpd.read_parquet("Data/" + city.replace(" ", "_") + "_" + state + ".parquet")
    else:
        place = city + ", " + state
        data = acs.from_place(place, place_type=placetype, variables=VARS)
    return data

def read_county_data(county, state):
    """
    Reads data for a given county and state from Data folder in parquet format
    [ADD LATER: If it doesn't exist, then calls census.write_county_data() to create it]

    Parameters:
        county (str): County name (e.g. "Bexar")
        state (str): State name (e.g. "TX")
    
    Returns:
        data (DataFrame): Data for given county and state
    """
    try:
        data = gpd.read_parquet("Data/" + county.replace(" ", "_") + "_" + state + ".parquet")
    except FileNotFoundError:
        write_county_data(county, state, VARS)
        data = gpd.read_parquet("Data/" + county.replace(" ", "_") + "_" + state + ".parquet")
    return data
    # return gpd.read_parquet("Data/" + county + "_County_" + state + ".parquet")

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

    # # Combine native, pacific , other, and two or more into "other"
    # gdf["other_non_hispanic"] = gdf["other_non_hispanic"] + gdf["native_non_hispanic"] + gdf["pacific_non_hispanic"] + gdf["two_or_more_non_hispanic"]
    # gdf = gdf.drop(columns=["native_non_hispanic", "pacific_non_hispanic", "two_or_more_non_hispanic"])

    # Drop rows with missing median income values
    gdf = gdf.dropna(subset=["median_income"])

    check_data_validity(gdf)

    # Remove total population from race variables
    gdf = gdf.drop(columns=["total_population_race"])

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
