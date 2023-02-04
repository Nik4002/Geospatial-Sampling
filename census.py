# %% Import packages
import cenpy as cen
import pandas as pd

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

def write_county_data(place, variables):
    """
    Writes data for a given county and variables to csv file

    Parameters:
        place (str): County name (e.g. "Bexar County, TX")
        variables (list): List of variables to include in data
    """
    acs = connect_acs()
    data = acs.from_place(place, variables=variables)
    data.to_csv("Data/" + place + ".csv", sep=" ")

# %% Write descriptions of variable groups to csv file
describe_vars("B03002")
describe_vars("B19001")

# %% Define the variables to include in the data
VARS = ["^B03002_00", # Variables in the B03002 group starting with B03002_00
    "B03002_010", 
    "B03002_011", 
    "B03002_012", 
    "^B19001"] # Variables in the B19001 group
