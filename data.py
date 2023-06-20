# %%
import pandas as pd
import geopandas as gpd
import cenpy as cen

def connect_acs():
    """
    Establishes connection to American Community Survey data API
    """
    acs = cen.products.ACS()
    return acs

VARS = ["B01003_001E",
    "B03002_001",
    "B03002_002",
    "B03002_003",
    "B03002_004",
    "B03002_005",
    "B03002_006",
    "B03002_007",
    "B03002_008",
    "B03002_009",
    "B03002_012",
    "B19013_001"
]

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

# def add_city(key, state, acs=connect_acs(), places=place_table(), strict_within=True):
#     """
#     Adds a city to parquet file called "cities.parquet"

#     Parameters:
#         key (str): Census API name for city (e.g. "San Antonio city")
#         city (str): Colloquial name (e.g. "San Antonio")
#         state (str): State name (e.g. "TX")
    
#     Returns:
#         None
#     """
#     # Read data from Census API
#     type = place_type(key, state, places)
    
#     city = colloquial(key)

#     data = get_city_data(key, state, type, acs=acs, strict_within=strict_within)
    
#     # Add place name columns
#     data["colloquial_name"] = city
#     data["place_name"] = key
#     data["state"] = state

#     # Check if data is empty
#     if data.empty:
#         print(f"{city}, {state} produced an empty dataframe")
#         return False
    
#     # Rank tracts by fips code and assign them values 1-10 based on their decile
#     data["region"] = (data["GEOID"].rank(pct=True) // 0.1 + 1).astype(int)
    
#     if strict_within == True:
#     # Read cities.parquet
#         try:
#             cities = gpd.read_parquet("cities.parquet")
#         except FileNotFoundError:
#             cities = gpd.GeoDataFrame()
#     else:
#         try:
#             cities = gpd.read_parquet("cities_loose.parquet")
#         except FileNotFoundError:
#             cities = gpd.GeoDataFrame()

#     # Create a copy of cities
#     cities_copy = cities.copy()

#     # Add city to cities.parquet using concat
#     # cities = gpd.concat([cities, data])
#     cities = gpd.GeoDataFrame(pd.concat([cities, data], ignore_index=True))

#     # Remove duplicates by GEOID, placename, and state
#     cities = cities.drop_duplicates(subset=["GEOID", "place_name", "state"])

#     # Check if cities.parquet has changed
#     if cities.equals(cities_copy):
#         print(f"{city}, {state} has already been added to file")
#         return False

#     # Write to file
#     if strict_within == True:
#         cities.to_parquet("cities.parquet")
#     else:
#         cities.to_parquet("cities_loose.parquet")

#     return True

def get_city_data(city, state, placetype, acs=connect_acs(), strict_within=True):
    """
    Reads and cleans data for a given city and state
    """
    # Read data
    data = read_city_data(city, state, placetype, acs=acs, strict_within=strict_within)

    # Clean data
    data = clean_data(data)

    return data

def read_city_data(city, state, placetype, acs=connect_acs(), strict_within=True):
    """
    Reads data for a given city and state from Data folder in parquet format

    Parameters:
        city (str): City name (e.g. "San Antonio city")
        state (str): State name (e.g. "TX")
        acs (census.ACS): ACS connection object
    
    Returns:
        data (DataFrame): Data for given city and state
    """
    place = city + ", " + state
    data = acs.from_place(place, place_type=placetype, variables=VARS, return_geometry=True, strict_within=strict_within)
    return data

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
        })
    gdf = gdf.drop(columns=["state", "county"])

    # Specify data types for each column

    # Add a column with the tract's centroid
    gdf["centroid"] = gdf.centroid

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
    # Assert that there are no duplicate values in the GEOID column
    assert not gdf["GEOID"].duplicated().any(), "Duplicate values in GEOID column"

    # Assert that there are no missing values
    assert not gdf.isnull().values.any(), "Missing values"

    # Check that race columns sum to total population
    non_hispanic = gdf[["white_non_hispanic", "black_non_hispanic", "asian_non_hispanic", "pacific_non_hispanic", "native_non_hispanic", "two_or_more_non_hispanic", "other_non_hispanic"]]


    assert non_hispanic.sum(axis=1).equals(gdf["total_non_hispanic"])
    assert gdf[["total_non_hispanic", "hispanic"]].sum(axis=1).equals(gdf["total_population_race"])
    assert gdf["total_population_race"].equals(gdf["total_population"])

def get_cities_data(strict_within=True):
    cities = pd.read_csv("city_list.csv")
    places = place_table()
    acs = connect_acs

    data = []
    for _, city in cities.iterrows():
        key = city["NAME"]
        state = state_abbrev(city["STNAME"])
        type = place_type(city["NAME"], state, places)
        colloq = colloquial(key)
        data_i = get_city_data(key, state, type, acs, strict_within=strict_within)
        data_i["colloquial_name"] = colloq
        data_i["place_name"] = key
        data_i["state"] = state
        data_i["region"] = (data_i["GEOID"].rank(pct=True) // 0.1 + 1).astype(int)
        data.append(data_i)

    data = pd.concat(data)
    return data

# %%
