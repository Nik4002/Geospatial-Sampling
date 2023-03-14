# %% Import packages
import cenpy as cen

# %% Define census querying functions
def connect_acs():
    """
    Establishes connection to American Community Survey data API
    """
    acs = cen.products.ACS()
    return acs

def describe_vars(variables):
    """
    Writes descriptions of variables within a particular group to csv file
    to Variable_Info folder
    """
    acs = connect_acs()
    vars = acs.filter_variables(variables)
    var_labels = vars["label"].rename("Label").drop_duplicates().sort_index()
    var_labels.to_csv("Variable_Info/" + str(variables) + "_vars.txt", sep=" ")

def write_county_data(county, state, variables):
    """
    Writes data for a given county and variables to csv file

    Parameters:
        place (str): County name (e.g. "Bexar")
        state (str): State name (e.g. "TX")
        variables (list): List of variables to include in data
    """
    acs = connect_acs()
    place = county + ", " + state
    data = acs.from_county(place, variables=variables)
    
    file_name = county + "_County_" + state + ".parquet"
    data.to_parquet("Data/" + file_name)

# %% Write descriptions of variable groups to csv file
# describe_vars("B03002")
# describe_vars("B19001B")
# describe_vars("B19013")
describe_vars("B01003")

# %% Define the variables to include in the data
VARS = ["B01003_001E",
    "B03002_001E",
    "B03002_002E",
    "B03002_003E",
    "B03002_004E",
    "B03002_005E",
    "B03002_006E",
    "B03002_007E",
    "B03002_008E",
    "B03002_009E",
    "B03002_012E",
    "B19013E_001E",
    # "B19001B_001E",
    # "B19001B_002E",
    # "B19001B_003E",
    # "B19001B_004E",
    # "B19001B_005E",
    # "B19001B_006E",
    # "B19001B_007E",
    # "B19001B_008E",
    # "B19001B_009E",
    # "B19001B_010E",
    # "B19001B_011E",
    # "B19001B_012E",
    # "B19001B_013E",
    # "B19001B_014E",
    # "B19001B_015E",
    # "B19001B_016E",
    # "B19001B_017E",
]

# %% Write data for particular county to csv file
write_county_data("Bexar", "TX", VARS)
write_county_data("Cook", "IL", VARS)

# %%
