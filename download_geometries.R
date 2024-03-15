# data management
library(dplyr)
library(tidyverse)
library(sf)

# data sources
library(tidycensus)
library(osmdata)

# viz
library(patchwork)
library(ggpattern)
library(pdftools)

# utilities
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
# options(tigris_use_cache = TRUE)

# Include functions
source("/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling/utils.R")

set.seed(seed = 100)


# Directory ---------------------------------------------------------------

# If debug is set to TRUE, outputs will be saved to the "debug" folder
debug <- FALSE

# Replace with your own paths
# wd_input = '/Users/nm/Desktop/Projects/work/skew-the-script/inputs.nosync'
# wd_output = '/Users/nm/Desktop/Projects/work/skew-the-script/outputs.nosync'
# wd_output = '/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling'
if (debug == FALSE) {
  wd <- '/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling'
} else {
  wd <- '/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling/Debug'
  main_dir <- '/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling'
}

# Spatial adjacency methods -----------------------------------------------

st_rook <- function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

# -------------------------------------------------------------------------

# census_api_key("YOUR KEY GOES HERE", install = TRUE)
readRenviron("~/.Renviron") # create an .Renviron > cd > touch .Renviron 

# # Check ACS attributes for 2020 and 5 year estimates
# census_data_dict <- load_variables(year = 2020, dataset = c('acs5'), cache = FALSE)

# Download Place data -----------------------------------------------------

places_pop_rank <- rank_places()

places_list <- get_place_list(places_pop_rank)

county_place_map <- join_counties(places_pop_rank, places_list)

# Run loop over places ----------------------------------------------------

# Vectors to keep track of Place IDs of cities that pass or fail the QC check
qc_fails <- c()
qc_passes <- c()

regions_all <- tibble()
syn_pop_all <- tibble()
samples_all <- tibble()
empty_tracts_all <- tibble()
city_borders_all <- tibble()

# Beginning of loop; filter clause is for if you want to start the loop in the middle
for (i in unique((county_place_map %>% filter(city_rank >= 61 & city_rank <= 100))$geoid)) {
  # i <- "1714000" # Chicago
  # i <- "3651000" # New York City
  # i <- "2255000" # New Orleans (wide city)
  # i <- "1571550" # Urban Honolulu
  # i <- "5548000" # Madison
  # i <- "2507000" # Boston
  # i <- "0820000" # Denver
  # i <- "0675000" # Stockton
  # i <- "0613392" # Chula Vista
  # i <- "0667000" # San Francisco (city with weird island)
  # i <- "4865000" # San Antonio
  # i <- "1304000" # Atlanta
  # i <- "0446000" # Mesa, Arizona
  # i <- "0603526" # Bakersfield, California
  # i <- "4837000" # Irving, Texas
  # i <- "0644000" # Los Angeles
  
  # Pull place name (ex: "San Antonio")
  place_name <- county_place_map %>% filter(geoid == i) %>% st_drop_geometry() %>%
    select(name_short) %>% pull() %>% unique()
  
  if (place_name == "Indianapolis city") {
    place_name = "Indianapolis"
  } else if (place_name == "Urban Honolulu") {
    place_name = "Honolulu"
  }
  
  state_abbrev <- places_pop_rank %>% filter(placeid == i) %>% select(abbrev) %>% pull()
  
  # Convert place name to snake case (ex: "san_antonio")
  place_name_lower <- paste0(gsub("\\s+|\\.|\\/", "_", tolower(place_name)), "_", tolower(state_abbrev))
  
  print(place_name)
  print(state_abbrev)
  
  # Get the place's geometry
  place_geo <- places_list %>% filter(geoid == i) %>%
    rename(placeid = geoid) %>% select(placeid, geometry) %>%
    fill_holes(threshold = 100000000) # Fill any holes in the city's border, even if this includes other cities
  
  # Get relevant FIPS codes
  city_fips <- county_place_map %>% filter(geoid == i) %>% st_drop_geometry() %>%
    select(state_code, county_code) 
  state_fips <- city_fips %>% select(state_code) %>% pull()
  county_fips <- city_fips %>% select(county_code) %>% pull()
  
  # Download tract geometries
  tract_data <- get_city_geoms(state_fips, county_fips, place_geo)
  
  # Save original city border
  city_border <- tract_data %>% st_union()
  
  # Save empty tracts
  empty_tracts <- tract_data %>%
    filter(is.na(total_population) | total_population == 0)
  
  # Remove empty tracts
  tract_data <- tract_data  %>%
    drop_na(total_population) %>%
    filter(total_population > 0)
  
  # Download data, recode variables to race / ethnicity categories, then aggregate
  
  tract_data_race <- get_race_data(state_fips, county_fips, tract_data)
  
  # Calculate each tract's plurality race
  tract_data_plurality_race <- calculate_plurality_race(tract_data_race)
  
  # Build spatial clusters --------------------------------------------------
  
  tract_data_clusters <- cluster_by_plurality(tract_data, tract_data_plurality_race)
  
  # Geometries --------------------------------------------------------------
  
  tract_data_race_shares <- calculate_race_shares(tract_data_race)
  
  # Median Household Income -------------------------------------------------
  
  tract_data_income <- get_income_data(state_fips, county_fips)
  
  # Combine data ------------------------------------------------------------
  
  # Join together tract data
  tract_data_all <- join_tract_data(tract_data, tract_data_race_shares, tract_data_plurality_race, tract_data_income)
  
  # -------------------------------------------------------------------------
  
  # Race distribution of city
  city_distribution <- calculate_point_distribution(tract_data_clusters, tract_data_all)
  
  # Generate cluster points using K-Means clustering
  sample_points <- generate_cluster_points(city_distribution, tract_data_clusters)
  
  ggplot() +
    geom_sf(data = tract_data_clusters, aes(fill = cluster_plurality_race)) +
    geom_sf(data = sample_points, color = 'black')
  
  # Use KNN to cluster tracts into 10 regions based on sample point locations
  tract_data_all_geo <- assign_regions(tract_data_all, sample_points)
  
  # Regional sample option
  ggplot() +
    geom_sf(data = tract_data_all_geo, aes(fill = cluster_id), color = 'white') +
    geom_sf(data = sample_points, color = 'black') +
    theme_void()
  
  # -------------------------------------------------------------------------
  
  # Create synthetic 100 households
  cluster_aggregate_data <- aggregate_by_cluster(tract_data_all_geo)
  
  # Create large, synthetic population representative of the city
  synthetic_distribution <- generate_syn_pop(cluster_aggregate_data, seed = 100)
  
  # Pull 100 people from synthetic population
  synthetic_sample <- pull_sub_pop_100(synthetic_distribution, tract_data_race)
  
  # Dissolve tracts into their regions
  clusters_10 <- dissolve_regions(tract_data_all_geo)
  
  # Calculate cluster boundary with a small inward buffer and remove water
  bbox <- calculate_expanded_bbox(city_border, buffer = 0.01)
  water_layer <- get_water(bbox, state_fips, county_fips)
  clusters_padding <- buffered_clusters(clusters_10, buffer = 200, water_layer)
  
  # Generate 100 dots per region
  dots <- region_dot_pop(clusters_padding)
  
  ggplot() +
    geom_sf(data = clusters_10, color = 'black', fill = 'white', alpha = 0, linewidth = .2) +
    geom_sf(data = dots, color = 'black', fill = 'black', alpha = 1, size = 1, linewidth = .2) +
    theme_void()
  
  # Cluster 100 dots into 10 sub-regions per region and select one dot per sub-region
  dots2 <- select_dots(clusters_padding, dots)
  
  # Join income/race data with dots
  synthetic_sample_points <- syn_pop_join(dots2, synthetic_sample)
  
  # Teacher's Key -----------------------------------------------------------
  
  num_students <- 50
  runs <- 10
  fail <- FALSE
  
  # Run the sampling functions [num_students] times and check if the samples pass the QC check;
  # if not, repeat [runs] times, and if it fails every time, add it to qc_fails
  for (j in 1:runs) {
    seed <- j
    # n students' samples concatenated into one dataframe and id'ed by student
    samples <- student_samples(num_students, seed, synthetic_sample_points)
    judgment_samples <- samples[[1]]
    simple_samples <- samples[[2]]
    stratified_samples <- samples[[3]]
    cluster_samples <- samples[[4]]
    
    # QC
    if (!check(pop = synthetic_sample_points, 
               simple_samples = simple_samples, 
               stratified_samples = stratified_samples, 
               cluster_samples = cluster_samples)) {
      if (j == runs) {
        fail = TRUE
      }
    } else {
      print(paste0(place_name, " passed the QC check on iteration ", j, "!"))
      break
    }
  }
  
  # If the city doesn't pass the "smell test", set fail to TRUE (list updated as of February 7th, 2024)
  if (place_name %in% c("Anaheim", "Anchorage", "Bakersfield", "Chesapeake", "Fremont", "Glendale", "Honolulu", "Lexington", "Louisville", "New Orleans", "Newark", "Orlando", "Seattle", "Stockton", "Tampa")) {
    fail = TRUE
  }
  
  if (fail == TRUE) {
    warning(sprintf("%s did not pass the QC check", place_name))
    qc_fails <- append(qc_fails, i)
    # next
  } else {
    qc_passes <- append(qc_passes, i)
    
    # Save info for the city
    regions_all <- clusters_10 %>% 
      mutate(city_geoid = i, city_name = place_name, state = state_abbrev, region_num = region_loc) %>%
      select(region_num, city_geoid, city_name, state, geometry) %>%
      rbind(regions_all, .) %>%
      distinct()
    
    syn_pop_all <- synthetic_sample_points %>% 
      mutate(city_geoid = i, city_name = place_name, state = state_abbrev, region_num = region_loc, income = median_household_income_noise) %>%
      select(id, race, income, region_num, city_geoid, city_name, state, geometry) %>%
      rbind(syn_pop_all, .) %>%
      distinct()
    
    samples_all <- rbind(
      samples[[1]] %>% mutate(sample_method = "Judgment"),
      samples[[2]] %>% mutate(sample_method = "Simple"),
      samples[[3]] %>% mutate(sample_method = "Stratified"),
      samples[[4]] %>% mutate(sample_method = "Cluster")
    ) %>%
      mutate(city_geoid = i, city_name = place_name, state = state_abbrev, region_num = region_loc, income = median_household_income_noise) %>%
      select(student_id, method, id, race, income, region_num, city_geoid, city_name, state) %>% 
      rbind(samples_all, .) %>%
      distinct()
      
    empty_tracts_all <- empty_tracts %>% 
      mutate(tract_id = geoid, city_geoid = i, city_name = place_name, state = state_abbrev) %>%
      select(tract_id, city_geoid, city_name, state, geometry) %>%
      rbind(empty_tracts_all, .) %>%
      distinct()
      
    city_borders_all <- city_border %>% 
      st_sf() %>%
      mutate(city_geoid = i, city_name = place_name, state = state_abbrev) %>%
      select(city_geoid, city_name, state, geometry) %>%
      rbind(city_borders_all, .) %>%
      distinct()
  }
  
} # End of loop (uncomment if looping)

samples_all_geom <- samples_all %>% 
  left_join(syn_pop_all %>% 
              select(id, city_geoid, geometry), 
            by = c("id", "city_geoid")) %>%
  st_as_sf()

# st_write(regions_all, paste0(wd, "/Geometries/regions.shp"), append = FALSE)
# st_write(syn_pop_all, paste0(wd, "/Geometries/syn_pops.shp"), append = FALSE)
# st_write(samples_all_geom, paste0(wd, "/Geometries/samples.shp"), append = FALSE)
# st_write(empty_tracts_all, paste0(wd, "/Geometries/empty_tracts.shp"), append = FALSE)
# st_write(city_borders_all, paste0(wd, "/Geometries/borders.shp"), append = FALSE)

st_write(regions_all, paste0(wd, "/Geometries/regions.geojson"), append = FALSE)
st_write(syn_pop_all, paste0(wd, "/Geometries/syn_pops.geojson"), append = FALSE)
st_write(samples_all_geom, paste0(wd, "/Geometries/samples.geojson"), append = FALSE)
st_write(empty_tracts_all, paste0(wd, "/Geometries/empty_tracts.geojson"), append = FALSE)
st_write(city_borders_all, paste0(wd, "/Geometries/borders.geojson"), append = FALSE)
