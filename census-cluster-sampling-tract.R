library(pdftools)

# data
library(dplyr)
library(tidyverse)
library(sf)
library(nngeo)
library(terra)
library(sf)
library(units)
library(smoothr)
# census
library(tidycensus)
library(tigris)
library(osmdata)
# viz
library(patchwork)
library(scales)
library(viridis)
library(gt)
library(gtExtras)
library(ggpmisc)
library(ggpattern)
# utilities
library(janitor)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
options(tigris_use_cache = TRUE)

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

# Sampling functions

judgment <- function(pop, seed = 100) { # Simulation of judgment sample; weighs random sampling by square of income (analogous to size of bubbles)
  set.seed(seed = seed)
  return(pop %>% slice_sample(n = 10, weight_by = median_household_income_noise ** 2))
}

simple_rs <- function(pop, seed = 100) { # Simple random sample; randomly samples 10 dots across the city
  set.seed(seed = seed)
  return(pop %>% slice_sample(n = 10))
}

stratified_rs <- function(pop, seed = 100) { # Stratified random sample; randomly samples one dot per region
  set.seed(seed = seed)
  return(pop %>% group_by(cluster_id) %>% slice_sample(n = 1) %>% ungroup())
}

cluster_rs <- function(pop, seed = 100) { # Cluster random sample; randomly samples one entire region
  set.seed(seed = seed)
  cluster <- unique(pop$cluster_id) %>% sample(., 1)
  return(pop %>% filter(cluster_id == cluster))
}

# QC check function; returns TRUE if pass, FALSE if fail
check <- function(pop, simple_samples, stratified_samples, cluster_samples) {
  # Aggregate samples into vectors of medians; one per student per sampling method
  simple_medians <- simple_samples %>% group_by(student_id) %>%
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median) %>% 
    pull()
  
  stratified_medians <- stratified_samples %>%
    group_by(student_id) %>%
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median) %>% 
    pull()
  
  cluster_medians <- cluster_samples %>%
    group_by(student_id) %>%
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median) %>% 
    pull()
  
  # Calculate variances and check rank
  simple_var <- var(simple_medians)
  stratified_var <- var(stratified_medians)
  cluster_var <- var(cluster_medians)
  
  if (stratified_var > simple_var | simple_var > cluster_var) {
    return(FALSE)
  }
  
  # Check bias; calculate distance from true median of 100 people to furthest median among the three sampling methods, then 
  # for each method, check if distance between true median and mean of medians for that method is over 25% of former distance
  true_median <- median(pop$median_household_income_noise)
  all_medians <- c(simple_medians, stratified_medians, cluster_medians)
  lo <- min(all_medians)
  hi <- max(all_medians)
  distance <- max(abs(lo - true_median), abs(hi - true_median))
  return(abs(true_median - mean(simple_medians))/distance <= 0.25 &
           abs(true_median - mean(stratified_medians))/distance <= 0.25 &
           abs(true_median - mean(cluster_medians))/distance <= 0.25)
}

# Spatial adjacency methods -----------------------------------------------

st_rook = function(a, b = a) st_relate(a, b, pattern = "F***1****")
st_queen <- function(a, b = a) st_relate(a, b, pattern = "F***T****")

# -------------------------------------------------------------------------

# census_api_key("YOUR KEY GOES HERE", install = TRUE)
readRenviron("~/.Renviron") # create an .Renviron > cd > touch .Renviron 
# Check ACS attributes for 2020 and 5 year estimates
census_data_dict <- load_variables(year = 2020, dataset = c('acs5'), cache = FALSE)

# Download Place data -----------------------------------------------------

# Read in Census Places (cities) population data
places_pop <- read_csv('https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/cities/totals/sub-est2022.csv')

# Read in state abbreviations
states <- read.csv(paste0(wd, "/states.csv"))

# Filter to largest 100 Places
places_pop_rank <- places_pop %>%
  rename_all(tolower) %>% 
  filter(sumlev %in% c('162')) %>%
  select(state, place, name, sumlev, stname, popestimate2020, popestimate2021, popestimate2022 ) %>%
  mutate(state = str_pad(state, width=2, side="left", pad="0"),
         place = str_pad(place, width=5, side="left", pad="0"),
         placeid = paste0(state,place)) %>%
  rename(cityname = name) %>%
  mutate(city_rank = row_number(desc(popestimate2022))) %>%
  filter(city_rank <= 100) %>%
  left_join(., states, by = join_by(stname == State)) %>%
  rename("abbrev" = "Abbreviation")
rm(places_pop)

places_list <- places(cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  inner_join(., places_pop_rank, by = c('GEOID'='placeid')) %>%
  rename_all(tolower) %>%
  select(geoid, name, popestimate2020, popestimate2021, popestimate2022, city_rank, geometry) %>% 
  separate(data = ., col = 'name', into = 'name_short', sep =  "(\\[|-|]|/|[(])", remove = TRUE, extra = "drop") # Clean up place names

# County/state codes for the entire country
state_xwalk <- tidycensus::fips_codes %>%
  mutate(county_fips = paste0(state_code,county_code))

# County geometries
us_county <- get_acs(year = 2020, geography = "county", variables = "B01003_001", 
                     geometry = TRUE, shift_geo = FALSE) %>%
  rename_all(tolower) %>% 
  select(geoid, estimate) %>%
  rename(county_fips = geoid) %>%
  left_join(., state_xwalk, by = c('county_fips' = 'county_fips')) %>% 
  select(state, county_fips, state_code, county_code) %>%
  st_transform(4326) 

# Counties with which 
county_place_map <- places_list %>%
  st_join(., us_county, left = FALSE) %>%
  arrange(desc(popestimate2022)) %>%
  mutate(name_short_strip = gsub("\\s+|\\.|\\/", "_", tolower(name_short)))

# System for removing cities that have already been processed
finished_list <- gsub(".pdf", "",list.files(path = wd, pattern = "\\.pdf$", full.names = FALSE))

remaining_list <- county_place_map %>% 
  filter(!(name_short_strip %in% finished_list))

# Run loop over places ----------------------------------------------------

# Vectors to keep track of Place IDs of cities that pass or fail the QC check
qc_fails <- c()
qc_passes <- c()

# Beginning of loop; filter clause is for if you want to start the loop in the middle
for (i in unique((remaining_list %>% filter(city_rank >= 61 & city_rank <= 100))$geoid)) {
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
  
  state_abbrev <- places_pop_rank %>% filter(placeid == i) %>% select(abbrev) %>% pull()
  
  # Convert place name to snake case (ex: "san_antonio")
  place_name_lower <- paste0(gsub("\\s+|\\.|\\/", "_", tolower(place_name)), "_", tolower(state_abbrev))
  
  print(place_name)
  
  # Get the place's geometry
  places_geo <- places_list %>% filter(geoid == i) %>%
    rename(placeid = geoid) %>% select(placeid, geometry) %>%
    fill_holes(threshold = 100000000) # Fill any holes in the city's border, even if this includes other cities
  
  # Get relevant FIPS codes
  city_fips <- county_place_map %>% filter(geoid == i) %>% st_drop_geometry() %>%
    select(state_code, county_code) 
  state_fips_2 <- city_fips %>% select(state_code) %>% pull()
  county_fips_3 <- city_fips %>% select(county_code) %>% pull()
  
  # Download tract geometries
  
  tract_data_geo <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", 
            survey = 'acs5', variables = c('B01003_001'),
            cache_table = TRUE, 
            state = x, county = y,
            geometry = TRUE) %>% 
      rename_all(list(tolower))  %>%
      select(geoid, estimate, geometry) %>%
      rename(total_population = estimate)
  })
  
  tract_data <- tract_data_geo %>%
    st_transform(4326) %>%
    st_join(x = ., y = places_geo %>% st_transform(3857) %>% 
              st_buffer(100) %>% st_transform(4326), 
            join = st_within, left = FALSE)
  
  # Handle special cases that cause errors
  if (i == "4865000") { # Removing remote military base from San Antonio
    tract_data <- tract_data %>% filter(geoid != "48029980005")
  } else if (i == "0820000") { # Removing airport from Denver
    tract_data <- tract_data %>% filter(geoid != "08031980001")
  } else if (i == "1304000") { # Removing Emory University from Atlanta
    tract_data <- tract_data %>% filter(geoid != "13089022404")
  } else if (i == "0667000") { # Removing remote island from San Francisco
    tract_data <- tract_data %>% filter(geoid != "06075980401")
  }
  
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
  
  tract_data_race <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", 
            survey = 'acs5', variables = c('B03002_012', 'B03002_003', 'B03002_004', 'B03002_005', 'B03002_006', 'B03002_007', 'B03002_008', 'B03002_009'),
            summary_var = 'B03002_001', 
            cache_table = TRUE, 
            state = x, county = y,
            geometry = FALSE) 
  })
    
  tract_data_race <- tract_data_race %>% 
    rename_all(list(tolower)) %>%
    mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino/a',
                                      variable == 'B03002_003' ~ 'White',
                                      variable == 'B03002_004' ~ 'Black',
                                      variable == 'B03002_005' ~ 'Native American',
                                      variable == 'B03002_006' ~ 'Asian/Pacific Islander',
                                      variable == 'B03002_007' ~ 'Asian/Pacific Islander',
                                      variable == 'B03002_008' ~ 'Multiracial/Other',
                                      variable == 'B03002_009' ~ 'Multiracial/Other',
                                      TRUE ~ as.character(''))) %>%
    group_by(geoid, variable_label, summary_est) %>% 
    summarize_at(.vars = vars(estimate), .funs = list(sum)) %>%
    ungroup() %>%
    filter(geoid %in% unique(tract_data$geoid))
  
  # Calculate each tract's plurality race
  tract_data_plurality_race <- tract_data_race  %>%
    mutate(plurality_race_share = estimate/summary_est) %>%
    group_by(geoid) %>%
    mutate(plurality_rank = row_number(desc(plurality_race_share))) %>%
    ungroup() %>%
    filter(estimate > 0) %>%
    select(geoid, variable_label, estimate, plurality_race_share, plurality_rank) %>%
    rename(plurality_race_population = estimate,
           plurality_race = variable_label) %>%
    filter(plurality_rank == 1) %>%
    select(geoid, plurality_race, plurality_race_population, plurality_race_share) 
  
  # Build spatial clusters --------------------------------------------------
  
  # Dissolve tract geometries by plurality race
  tract_data_grouped <- tract_data %>%
    left_join(., tract_data_plurality_race, by = c('geoid' = 'geoid')) %>%
    filter(!is.na(plurality_race))  %>%
    st_make_valid() %>%
    group_by(plurality_race) %>% 
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() %>% 
    st_cast("MULTIPOLYGON") %>% st_cast("POLYGON") %>%
    st_transform(3395) %>%
    mutate(area = st_area(.)) %>%
    st_transform(4326) %>%
    group_by(plurality_race) %>%
    mutate(area_rank = row_number(desc(area))) %>%
    ungroup() %>%
    mutate(area_share = as.numeric(area/sum(area)))%>%
    filter(area_rank <= 1 | area_share >= .01) %>% # must have area over 1% or 1 cluster per race
    select(plurality_race, geometry) 

  ggplot() +
    geom_sf(data = tract_data_grouped, aes(fill = plurality_race))
  
  # Join dissolved clusters to tract data so every tract is assigned to its closest cluster
  tract_data_clusters <- tract_data %>% 
    st_make_valid() %>%
    select(geoid, total_population, geometry) %>%
    # within, rook
    st_join(x = ., y = tract_data_grouped %>% st_make_valid(), left = TRUE, largest = TRUE) %>% 
    #st_join(x = ., y = tract_data_grouped %>% st_make_valid(), join = st_nn)  %>%
    st_join(x = ., y = tract_data_grouped %>% st_make_valid(), join = st_rook)  %>%
    mutate(plurality_race = coalesce(plurality_race.x, plurality_race.y))
  
  ggplot() +
    geom_sf(data = tract_data_clusters, aes(fill = plurality_race))
  
  # Re-dissolve tract map into complete cluster map
  tract_data_clusters_grouped <- tract_data_clusters %>%
    select(plurality_race, geometry) %>%
    group_by(plurality_race) %>%
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() %>% 
    st_make_valid() %>%
    st_cast(., "POLYGON") %>%
    arrange(plurality_race) %>%
    mutate(cluster_group = row_number()) 
  
  ggplot() +
    geom_sf(data = tract_data_clusters_grouped, aes(fill = plurality_race), color = 'white')

  # Re-join dissolved clusters to tract data so every tract is assigned to its closest cluster
  tract_data_clusters <- tract_data %>% st_make_valid() %>%
    select(geoid, total_population, geometry) %>%
    # within, rook
    st_join(x = ., y = tract_data_clusters_grouped %>% st_make_valid(), left = TRUE, largest = TRUE) %>% 
    st_join(x = ., y = tract_data_clusters_grouped %>% st_make_valid(), join = st_nn)  %>%
    mutate(cluster_plurality_race = coalesce(plurality_race.x, plurality_race.y),
           cluster_group = coalesce(cluster_group.x, cluster_group.y)) %>%
    select(geoid, total_population, cluster_plurality_race, cluster_group, geometry) %>%
    st_transform(4326)
  
  ggplot() +
     geom_sf(data = tract_data_clusters, aes(fill = cluster_plurality_race))

  # Geometries --------------------------------------------------------------
  
  # Calculate summary statistics
  tract_data_race <- tract_data_race %>%
    mutate(race_share = estimate/summary_est) %>%
    group_by(geoid) %>%
    mutate(plurality_rank = row_number(desc(race_share))) %>%
    ungroup() %>%
    #filter(estimate > 0) %>%
    select(geoid, variable_label, estimate, race_share, plurality_rank) %>%
    rename(race_population = estimate,
           race = variable_label)
  
  # Create wide dataframe of race shares for each tract
  tract_data_race_wide <-  tract_data_race %>%
    pivot_wider(id_cols = c(geoid),
                names_from = c(race),
                values_from = c(race_share, race_population),
                values_fill = 0) %>%
    rename_all(list(tolower)) %>%
    select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
    select(geoid, race_share_asian_pacific_islander, race_share_black, race_share_latino_a, race_share_white, race_share_multiracial_other, race_share_native_american, 
           race_population_asian_pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial_other, race_population_native_american)
  
  # Median Household Income -------------------------------------------------
  
  # Download data for tracts
  tract_data_income <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", 
                               survey = 'acs5', variables = c('B19013_001'),
                               summary_var = 'B01003_001',
                               cache_table = TRUE,
                               state = x, county = y,
                               geometry = FALSE)
  })
  
  tract_data_income <- tract_data_income %>%
    mutate(variable_label = case_when(variable == 'B19013_001' ~ 'Median household income in the past 12 months')) %>%
    rename_all(list(tolower)) %>%
    select(geoid, estimate, summary_est) %>%
    rename(total_population = summary_est,
           median_household_income = estimate) %>%
    mutate(income_median = median(median_household_income, na.rm = TRUE),
           median_household_income = case_when(is.na(median_household_income)  ~ income_median,
                                               TRUE ~ as.integer(median_household_income))) %>%
    select(geoid, total_population, median_household_income) %>%
    mutate(household_income_bucket = case_when(median_household_income < 25000 ~ '1 - $0-25k',
                                               median_household_income >= 25000 & median_household_income < 50000 ~ '2 - $25-50k',
                                               median_household_income >= 50000 & median_household_income < 75000 ~ '3 - $50-75k',
                                               median_household_income >= 75000 & median_household_income < 100000 ~ '4 - $75-100k',
                                               median_household_income >= 100000 & median_household_income < 150000 ~ '5 - $100-150k',
                                               median_household_income >= 150000 ~ '6 - $150k+'))
  
  # Combine data ------------------------------------------------------------
  
  # Join together tract data
  tract_data_all <- tract_data %>%
    left_join(., tract_data_race_wide, by = c('geoid'='geoid')) %>%
    left_join(., tract_data_plurality_race, by = c('geoid'='geoid')) %>%
    left_join(., tract_data_income %>% select(geoid, median_household_income, household_income_bucket), by = c('geoid'='geoid')) %>%
    select( geoid, total_population, median_household_income, household_income_bucket, plurality_race, plurality_race_population, plurality_race_share, 
            race_share_asian_pacific_islander, race_share_black, race_share_latino_a, race_share_white, race_share_multiracial_other, race_share_native_american, 
            race_population_asian_pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial_other, race_population_native_american,
            geometry) %>% 
    st_transform(4326) %>%
    mutate(race_sum = race_population_asian_pacific_islander + race_population_black + race_population_latino_a + race_population_white + race_population_multiracial_other + race_population_native_american) %>%
    mutate(total_population = case_when(is.na(total_population) ~ race_sum,
                                        TRUE ~ as.numeric(total_population))) 
   

  # -------------------------------------------------------------------------
  
  # Race distribution of city
  plurality_tract_race_list <- tract_data_clusters %>% st_drop_geometry() %>% 
    select(cluster_plurality_race) %>% distinct() %>% pull()
  
  plurality_tract_count <- tract_data_all %>% st_drop_geometry() %>%
    filter(!is.na(plurality_race)) %>%
    group_by(plurality_race) %>%
    tally() %>%
    ungroup() %>%
    rename(tract_count = n)
  
  # Calculate how many cluster points should be assigned to each race group
  city_distribution <- tract_data_all %>% st_drop_geometry() %>%
    summarize_at(vars(race_population_asian_pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial_other, race_population_native_american), 
                 list(sum)) %>%
    pivot_longer(cols =c("race_population_asian_pacific_islander", "race_population_black", "race_population_latino_a", "race_population_white", "race_population_multiracial_other", "race_population_native_american"),
                 names_to = c("race")) %>%
    mutate(race = case_when(race == "race_population_latino_a" ~ 'Latino/a',
                            race == "race_population_white" ~ 'White',
                            race == "race_population_black" ~ 'Black',
                            race == "race_population_native_american" ~ 'Native American',
                            race == "race_population_asian_pacific_islander" ~ 'Asian/Pacific Islander',
                            race == "race_population_multiracial_other" ~ 'Multiracial/Other'))  %>%
    filter(race %in% plurality_tract_race_list) %>% ## FILTERS OUT RACES WITHOUT A PLURALITY TRACT -- ONE FIX IS USE BLOCK GROUPS INSTEAD OF TRACTS FOR REGIONALIZATION
    left_join(., plurality_tract_count, by = c('race'='plurality_race')) %>%
    mutate(share = value / sum(value),
           count = round(share * 100, -1)/10,
           adjust = case_when(count > tract_count ~ tract_count,
                     TRUE ~ NA_integer_),
           count_adjust = case_when(count <= tract_count ~ count,
                              TRUE ~ NA_integer_),
           cluster_count = coalesce((10 - sum(adjust, na.rm = TRUE) ) * (count_adjust / sum(count_adjust, na.rm = TRUE)) , adjust),
           cluster_count = round(cluster_count,0)) %>%
    filter(cluster_count > 0) %>%
    mutate(residual = 10 - sum(cluster_count),
           tract_count_rank = row_number(desc(tract_count)),
           cluster_count = case_when(tract_count_rank == 1 ~ residual + cluster_count, 
                     TRUE ~ as.integer(cluster_count))) %>%
    uncount(cluster_count) %>%
    sample_n(size = 10) %>% # come up with way to force minority clusters to always be sampled
    group_by(race) %>%
    tally(name = 'cluster_count') %>%
    ungroup() 
  
  # Generate cluster points usign K-Means clustering
  sample_points <- purrr::map_dfr(
    .x = city_distribution %>% select(race) %>% pull(), 
    .f = function(i) {

      print(i)
      
      (num_points <- city_distribution %>%
        filter(race == i) %>% 
        select(cluster_count) %>% pull() )
      
      # If trying to sample n points from a pool of n using K-Means, the default algorithm (Hartigan-Wong) doesn't work 
      if (num_points > 1) { 
        if (num_points == nrow(tract_data_clusters %>%
                               filter(cluster_plurality_race == i))) {
          algorithm <- "Lloyd"
        } else {
          algorithm <- "Hartigan-Wong"
        }
        
        # Population-weighted XY k-means
        set.seed(seed = 100)
        k_clusters <- stats::kmeans(x = tract_data_clusters %>%
                                      filter(cluster_plurality_race == i) %>%
                                      #st_transform(3395) %>%
                                      st_centroid() %>%
                                      mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]) ,
                                             lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]) ) %>%
                                      st_drop_geometry() %>%
                                      mutate(total_population = replace_na(total_population, 0)) %>%
                                      mutate(lon = scale(lon, center = TRUE, scale = TRUE),
                                             lat = scale(lat, center = TRUE, scale = TRUE)) %>%
                                      select(lon, lat) %>%
                                      as.matrix(),
                                    centers = num_points,
                                    algorithm = algorithm)
      } else {
        k_clusters <- tract_data_clusters %>%
          filter(cluster_plurality_race == i) %>%
          mutate(cluster = 1) %>% select(cluster)
      }
      
      # # XY-only k-means
      # k_clusters <- stats::kmeans(x = bg_data_clusters %>%
      #                               filter(cluster_plurality_race == i) %>%
      #                               st_centroid() %>% st_coordinates(),
      #                             centers = num_points)
      
      race_tract_groups <- tract_data_clusters %>% 
        filter(cluster_plurality_race == i) %>%
        mutate(cluster_race = k_clusters$cluster) %>%
        st_make_valid() %>%
        group_by(cluster_race) %>%
        dplyr::summarize(geometry = st_union(geometry)) %>%
        ungroup() %>%
        st_point_on_surface() %>%
        mutate(
          cluster_id_race = i,
          cluster_id = paste0(gsub("\\s+|\\.|\\/|-|,|\\+", "", tolower(i)),'_',cluster_race)) %>%
        select(cluster_id_race, cluster_id,  geometry) %>% 
        st_transform(4326) 
    })
  
  ggplot() +
    geom_sf(data = tract_data_clusters, aes(fill = cluster_plurality_race)) +
    geom_sf(data = sample_points, color = 'black')
  
  tract_data_sample <- sample_points %>%
    st_transform(3395) %>%
    st_join(., tract_data_all %>% st_transform(3395), left = TRUE,
            join = st_nn, k = 10) %>% #, returnDist = TRUE
    st_transform(4326)
  
  # Use KNN to cluster tracts into 10 regions based on sample point locations
  tract_data_all_geo <- tract_data_all %>% st_transform(3395) %>%
    st_join(., sample_points %>% st_transform(3395), left = TRUE,
            join = st_nn, k = 1) %>% 
    st_transform(4326)

  # Regional sample option
  ggplot() +
    geom_sf(data = tract_data_all_geo, aes(fill = cluster_id), color = 'white') +
    geom_sf(data = sample_points, color = 'black') +
    theme_void()
  
  # Roads and water ---------------------------------------------------------
  
  # Calculate expanded bounding box (e.g. if buffer = 0.01, bounding box is expanded by 1% vertically and horizontally)
  buffer <- 0.01
  bbox <- city_border %>% st_bbox()
  width <- bbox$xmax - bbox$xmin
  height <- bbox$ymax - bbox$ymin
  bbox$xmax = bbox$xmax + buffer * width 
  bbox$xmin = bbox$xmin - buffer * width
  bbox$ymax = bbox$ymax + buffer * height
  bbox$ymin = bbox$ymin - buffer * height
  rm(width, height, buffer)
  
  bbox <- st_sfc(st_polygon(list(rbind(c(bbox$xmax, bbox$ymax), 
                                       c(bbox$xmax, bbox$ymin), 
                                       c(bbox$xmin, bbox$ymin), 
                                       c(bbox$xmin, bbox$ymax), 
                                       c(bbox$xmax, bbox$ymax)))))
  st_crs(bbox) <- st_crs(4326)

  # Pull green space, roads, and water as basemap layers and intersect with expanded bbox
  if (i != "3651000" & i != "0644000") {
    green_spaces <- opq(bbox = bbox, timeout = 180) %>%
      add_osm_feature(key = 'leisure', value = 'park') %>%
      osmdata_sf()
    if (is.null(green_spaces$osm_multipolygons)) {
      if (is.null(green_spaces$osm_polygons)) {
        green_layer <- st_sf(st_sfc())
      } else {
        green_layer <- green_spaces$osm_polygons %>% select(osm_id) %>% st_make_valid() %>% st_intersection(., bbox) %>% st_union()
      }
    } else {
      if (is.null(green_spaces$osm_polygons)) {
        green_layer <- green_spaces$osm_multipolygons %>% select(osm_id) %>% st_make_valid() %>% st_intersection(., bbox) %>% st_union()
      } else {
        green_layer <- rbind(green_spaces$osm_polygons %>% select(osm_id),
                             green_spaces$osm_multipolygons %>% select(osm_id)) %>% st_make_valid() %>% st_intersection(., bbox) %>% st_union()
      }
    }
  }

  roads_layer <- tigris::primary_roads(year = 2020) %>%
    st_transform(4326) %>% 
    st_intersection(., bbox)
  
  secondary_roads_layer <- tigris::primary_secondary_roads(year = 2020, state = places_pop_rank %>% filter(placeid == i) %>% select(state) %>% pull()) %>%
    st_transform(4326) %>% 
    st_intersection(., bbox)
  
  water_layer <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    water_layer <- tigris::area_water(state = x, county = y, year = 2020) %>%
      st_transform(4326) %>% 
      st_intersection(., bbox)
    })
  
  # -------------------------------------------------------------------------
  
  # Create synthetic 100 households
  cluster_aggregate_data <- tract_data_all_geo %>%
    st_drop_geometry() %>%
    group_by(cluster_id_race, cluster_id, median_household_income) %>%
    summarize_at(vars(race_population_asian_pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial_other, race_population_native_american), 
                 list(sum)) %>%
    ungroup() %>%
    pivot_longer(cols =c("race_population_asian_pacific_islander", "race_population_black", "race_population_latino_a", "race_population_white", "race_population_multiracial_other", "race_population_native_american"),
                 names_to = c("race")) %>%
    mutate(race = case_when(race == "race_population_latino_a" ~ 'Latino/a',
                            race == "race_population_white" ~ 'White',
                            race == "race_population_black" ~ 'Black',
                            race == "race_population_native_american" ~ 'Native American',
                            race == "race_population_asian_pacific_islander" ~ 'Asian/Pacific Islander',
                            race == "race_population_multiracial_other" ~ 'Multiracial/Other')) %>%
    mutate(median_household_income_wt = median_household_income * value) %>%
    group_by(cluster_id_race, cluster_id, race) %>%
    summarize_at(vars(median_household_income_wt, value),
                 list(sum)) %>%
    ungroup() %>%
    mutate(median_household_income = median_household_income_wt/value) %>%
    select(-one_of(c('median_household_income_wt'))) %>%
    group_by(cluster_id) %>%
    mutate(share = value/sum(value),
           count = case_when(share < .01 ~ ceiling(share * 102),
                             share >= .01 ~ round(share*102, 0))) %>%
    ungroup()
  
  # Create large, synthetic population representative of the city
  set.seed(seed = 100)
  synthetic_distribution <- cluster_aggregate_data %>%
    type.convert(as.is = TRUE) %>% 
    uncount(value) %>%
    mutate(random_decimal = runif(n = nrow(.), min = 0, max = 1),
           random_normal = 1 + rnorm(n = nrow(.), sd = .1)) %>%
    group_by(cluster_id) %>%
    mutate(rank_random = row_number(desc(random_decimal))) %>%
    ungroup() %>% 
    group_by(race) %>%
    mutate(race_count = n()) %>%
    ungroup() %>%
    mutate(median_household_income_noise = median_household_income * random_normal,
           residual =  median_household_income - median_household_income_noise) %>%
    group_by(race, cluster_id) %>%
    mutate(sum_residual = sum(residual),
           count_residual = sum(n()),
           adj_residual = sum_residual / count_residual) %>%
    ungroup() %>%
    mutate(median_household_income_noise = median_household_income_noise + adj_residual) %>%
    select(cluster_id_race, cluster_id, race, median_household_income_noise, rank_random) 
  
  # Pull 100 people from synthetic population
  synthetic_sample <- synthetic_distribution %>%
    filter(rank_random <= 10) %>%
    group_by(cluster_id, race) %>%
    mutate(freq_race = sum(n())) %>%
    ungroup() %>%
    group_by(cluster_id) %>%
    mutate(synth_id = row_number(desc(freq_race))) %>%
    ungroup() %>%
    select(cluster_id_race, cluster_id, synth_id, race, median_household_income_noise)
  
  # Ensure that all race groups are represented in the 100-person sample
  (missing_races <- setdiff(tract_data_race %>% select(race) %>% distinct() %>% pull(),
                            synthetic_sample %>% select(race) %>% distinct() %>% pull()
  ))
  
  if (length(missing_races) > 0 ) {
    print("Detected missing race / ethnicity.")
    
    (synthetic_min_quota <- synthetic_distribution %>% 
        group_by(race) %>% sample_n(1) %>%
        ungroup() %>%
        filter(race %in% missing_races))
    
    if (nrow(synthetic_min_quota) > 0 ) {
      print("Adding in missing race / ethnicity.")
      
      synthetic_minority_substitution <- synthetic_sample %>% 
        filter(cluster_id %in% c(synthetic_min_quota %>% select(cluster_id) %>% pull()),
               race %in% c(synthetic_min_quota %>% select(cluster_id_race) %>% pull())) %>%
        sample_n(1) %>%
        select(cluster_id, synth_id) %>% 
        mutate(drop_row = 1) 
      
      synthetic_min_quota <- synthetic_min_quota %>%
        mutate(synth_id = synthetic_minority_substitution$synth_id)
      
      synthetic_sample <- synthetic_sample %>%
        left_join(., synthetic_minority_substitution, by = c('cluster_id'='cluster_id',
                                                             'synth_id'='synth_id')) %>%
        filter(is.na(drop_row)) %>%
        bind_rows(synthetic_min_quota) %>%
        select(cluster_id_race, cluster_id, synth_id, race, median_household_income_noise)
      
    } else { 
      print('Race / ethnicity absent from synthetic count data')
    }
  } else { 
    print('No missing race / ethnicities.') 
  }
  
  # Map ---------------------------------------------------------------------
  
  # Dissolve tracts into their regions
  clusters_10 <- tract_data_all_geo %>% 
    group_by(cluster_id) %>% # cluster_group,
    dplyr::summarize(geometry = st_union(geometry)) %>%
    ungroup() %>%
    mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]) ,
           lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
    mutate(lon_tile = ntile(-desc(lon), 5),
           lat_tile = ntile(desc(lat), 5)) %>%
    arrange(lat_tile, lon_tile, -desc(lon)) %>%
    mutate(region_loc = row_number())
  
  # Calculate cluster boundary with a small inward buffer
  clusters_padding <- clusters_10 %>%
    st_difference(., clusters_10 %>% 
                    st_boundary() %>% st_as_sf() %>%
                    st_transform(3395) %>%
                    st_buffer(., dist = 200) %>%
                    st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_make_valid() ) 
  
  water_remove <- water_layer %>% st_transform(3395) %>%
    st_buffer(., dist = 10) %>%
    st_simplify(., preserveTopology = TRUE, dTolerance = units::set_units(10,m)) %>% 
    st_union(.) %>% st_as_sf() %>%
    st_transform(4326) %>% st_make_valid()
  
  # If water remains after filtering, remove it from clusters_padding so no people can generate there
  if (nrow(water_remove) > 0 ) {
    clusters_padding <- clusters_padding %>%
      st_difference(., water_layer %>% st_transform(3395) %>%
                      st_buffer(., dist = 10) %>%
                      st_simplify(., preserveTopology = TRUE, dTolerance = units::set_units(10,m)) %>% 
                      st_union(.) %>%
                      st_transform(4326) %>% st_make_valid())
  }
  
  # Generate 100 dots per region
  set.seed(seed = 100)
  dots <- sf::st_sample(clusters_padding %>% st_transform(3395), size = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100), by_polygon = TRUE, type = 'regular') %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    st_join(., clusters_10, left = FALSE,  join = st_within) # , largest = TRUE
  
  ggplot() +
    geom_sf(data = clusters_10, color = 'black', fill = 'white', alpha = 0, linewidth = .2) +
    geom_sf(data = dots, color = 'black', fill = 'black', alpha = 1, size = 1, linewidth = .2) +
    theme_void()
  
  # Cluster 100 dots into 10 sub-regions per region and select one dot per sub-region
  dots2 <- purrr::map_dfr(
    .x = clusters_padding %>% st_drop_geometry() %>% select(cluster_id) %>% pull(), 
    .f = function(i) {
      print(i)
      
      # Population-weighted XY k-means
      k_clusters <- stats::kmeans(x = dots %>%
                                    filter(cluster_id == i) %>%
                                    mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]) ,
                                           lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]) ) %>%
                                    st_drop_geometry() %>%
                                    select(lon, lat) %>%
                                    as.matrix(),
                                  centers = 10)
      
      output <- dots %>% 
        filter(cluster_id == i) %>%
        mutate(synth_id = k_clusters$cluster) %>%
        st_make_valid() %>%
        group_by(region_loc, cluster_id, synth_id) %>% # cluster_group,
        dplyr::summarize(geometry = st_union(geometry)) %>%
        ungroup() %>%
        st_point_on_surface() 
    })
  
  # Join income/race data with dots
  synthetic_sample_points <- dots2  %>% 
    left_join(., synthetic_sample, by = c('cluster_id' = 'cluster_id', 
                                          'synth_id' = 'synth_id')) %>%
    mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]) ,
           lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
    arrange(region_loc, -desc(lon)) %>%
    mutate(id = row_number()) 
  
  # Color scheme for maps (follows races in alphabetical order)
  color_vec <- c('#F5870C', '#4472C4', '#06c049', '#70309F', '#fece0a', '#FF0000')
  
  # Generate map
  if (i != "3651000" & i != "0644000") {
    (map <- ggplot() +
        geom_sf(data = bbox, fill = 'white', alpha = 1) + # White background
        geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
        geom_sf(data = green_layer, fill = '#c6f2c2', color = '#ffffffff') +
        geom_sf(data = secondary_roads_layer, color = '#fae7af', alpha = 1, linewidth = .4) +
        geom_sf(data = roads_layer, color = '#fae7af', alpha = 1, linewidth = .6) +
        geom_sf_pattern(data = empty_tracts %>% st_union(), pattern = 'stripe', pattern_fill = '#eeeeee', pattern_colour = '#999999', alpha = 0.5, pattern_density = 0.5, pattern_angle = 45, pattern_spacing = 0.025) +
        geom_sf(data = city_border, color = '#999999', alpha = 0, linewidth = .6) +
        geom_sf(data = bbox, color = '#999999', alpha = 0, linewidth = 1) + # Map border
        geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
        geom_sf(data = synthetic_sample_points %>% # 100 sample points
                  mutate(race = case_when(race == 'Asian/Pacific Islander' ~ 'Asian',
                                          race == 'Multiracial/Other' ~ 'Multiracial',
                                          race == 'Native American' ~ 'Native',
                                          TRUE ~ as.character(race))) %>%
                  mutate(race = factor(race, levels = c("Asian", "Black", "Latino/a", "Multiracial", "Native", "White"))),
                aes(color = race, fill = race, size = median_household_income_noise ),
                alpha = .8, linewidth = .2) +
        ggrepel::geom_text_repel(data = synthetic_sample_points, # Point labels
                                 seed = 1, segment.curvature = 0, point.padding = 0, box.padding = 0, max.iter = 1000, segment.square  = FALSE, segment.inflect = FALSE,
                                 min.segment.length = 0, max.overlaps = Inf, force = .01, force_pull = 2, aes(x = lon, y = lat, label = id),
                                 size = 3, vjust =.5, color = 'white', fontface='bold') +
        guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
        scale_fill_manual(values = color_vec, name = 'Race/\nethnicity*') +
        scale_color_manual(values = color_vec, name = 'Race/\nethnicity*' ) +
        scale_size_binned(name = 'Household\nincome', range = c(2.5, 12),
                          n.breaks = 4,
                          labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
        guides(color = guide_legend(override.aes = list(size = 6, alpha = 1))) +
        labs(title = place_name,
             subtitle = paste0("100 representative people in 10 regions in ", place_name, " (10 per region)"),
             caption = "*Complete race/ethnicity names from U.S. Census:
                          Asian: Asian, Native Hawaiian and Other Pacific Islander; Black: Black or African American;
                          Latino/a: Hispanic or Latino; Multiracial: Two or more races, Other races;
                          Native: American Indian and Alaska Native; White: White.") +
        theme_void() + theme(legend.position = 'right',
                             legend.justification = "top",
                             plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
                             plot.subtitle = element_text(size = 12, hjust = 0.5),
                             plot.caption = element_text(size = 10, hjust = 0.5, vjust = 0.5),
                             legend.title = element_text(size = 12, face = 'bold', hjust = 0),
                             legend.text = element_text(size = 12),
                             legend.margin=margin(t=20,r=0,b=0,l=5),
                             legend.box.margin=margin(0,5,0,0),
                             plot.margin=unit(c(t=10,r=0,b=10,l=10), "pt"),
                             legend.box = 'vertical'
        ))
  } else {
    (map <- ggplot() +
       geom_sf(data = bbox, fill = 'white', alpha = 1) + # White background
       geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
       # geom_sf(data = green_layer, fill = '#c6f2c2', color = '#ffffffff') +
       geom_sf(data = secondary_roads_layer, color = '#fae7af', alpha = 1, linewidth = .4) +
       geom_sf(data = roads_layer, color = '#fae7af', alpha = 1, linewidth = .6) +
       
       geom_sf(data = empty_tracts %>% st_union(), fill = '#eeeeee', alpha = 0.5) + 
       # geom_sf_pattern(data = empty_tracts %>% st_union(), pattern = 'stripe', pattern_fill = '#eeeeee', pattern_colour = '#999999', alpha = 0.5, pattern_density = 0.5, pattern_angle = 45, pattern_spacing = 0.025) +
       geom_sf(data = city_border, color = '#999999', alpha = 0, linewidth = .6) +
       geom_sf(data = bbox, color = '#999999', alpha = 0, linewidth = 1) + # Map border
       geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
       geom_sf(data = synthetic_sample_points %>% # 100 sample points
                 mutate(race = case_when(race == 'Asian/Pacific Islander' ~ 'Asian',
                                         race == 'Multiracial/Other' ~ 'Multiracial',
                                         race == 'Native American' ~ 'Native',
                                         TRUE ~ as.character(race))) %>%
                 mutate(race = factor(race, levels = c("Asian", "Black", "Latino/a", "Multiracial", "Native", "White"))),
               aes(color = race, fill = race, size = median_household_income_noise ),
               alpha = .8, linewidth = .2) +
       ggrepel::geom_text_repel(data = synthetic_sample_points, # Point labels
                                seed = 1, segment.curvature = 0, point.padding = 0, box.padding = 0, max.iter = 1000, segment.square  = FALSE, segment.inflect = FALSE,
                                min.segment.length = 0, max.overlaps = Inf, force = .01, force_pull = 2, aes(x = lon, y = lat, label = id),
                                size = 3, vjust =.5, color = 'white', fontface='bold') +
       guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
       scale_fill_manual(values = color_vec, name = 'Race/\nethnicity*') +
       scale_color_manual(values = color_vec, name = 'Race/\nethnicity*' ) +
       scale_size_binned(name = 'Household\nincome', range = c(2.5, 12),
                         n.breaks = 4,
                         labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
       guides(color = guide_legend(override.aes = list(size = 6, alpha = 1))) +
       labs(title = place_name,
            subtitle = paste0("100 representative people in 10 regions in ", place_name, " (10 per region)"),
            caption = "*Complete race/ethnicity names from U.S. Census:
                          Asian: Asian, Native Hawaiian and Other Pacific Islander; Black: Black or African American;
                          Latino/a: Hispanic or Latino; Multiracial: Two or more races, Other races;
                          Native: American Indian and Alaska Native; White: White.") +
       theme_void() + theme(legend.position = 'right',
                            legend.justification = "top",
                            plot.title = element_text(size = 15, face = 'bold', hjust = 0.5),
                            plot.subtitle = element_text(size = 12, hjust = 0.5),
                            plot.caption = element_text(size = 10, hjust = 0.5, vjust = 0.5),
                            legend.title = element_text(size = 12, face = 'bold', hjust = 0),
                            legend.text = element_text(size = 12),
                            legend.margin=margin(t=20,r=0,b=0,l=5),
                            legend.box.margin=margin(0,5,0,0),
                            plot.margin=unit(c(t=10,r=0,b=10,l=10), "pt"),
                            legend.box = 'vertical'
       ))
  }

  design1 <- "
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
  "

  map <- map + guide_area() + plot_layout(design = design1) # Collect all guides to the right of the map

  map

  edges <- st_bbox(clusters_10)
  if (edges$xmax - edges$xmin > edges$ymax - edges$ymin) { # If width is greater than height, save as a landscape PDF and rotate
    ggsave(plot = map,
           filename = paste0(wd,'/Plots/', place_name_lower, '_map_landscape', '.pdf'),
           width = 11, height = 8.5) # dpi = 300,
    qpdf::pdf_rotate_pages(input = paste0(wd,'/Plots/', place_name_lower, '_map_landscape', '.pdf'),
                           pages = c(1),
                           angle = 90,
                           output = paste0(wd,'/Plots/', place_name_lower, '_map', '.pdf'))
  } else { # Otherwise, save as a portrait PDF
    ggsave(plot = map,
           filename = paste0(wd,'/Plots/', place_name_lower, '_map', '.pdf'),
           width = 8.5, height = 11) # dpi = 300,
  }
  
  # st_write(bbox, paste0(main_dir, "/Cache/", place_name_lower, "_bbox.shp"))
  # st_write(water_layer, paste0(main_dir, "/Cache/", place_name_lower, "_water.shp"))
  # st_write(roads_layer, paste0(main_dir, "/Cache/", place_name_lower, "_roads.shp"))
  # st_write(secondary_roads_layer, paste0(main_dir, "/Cache/", place_name_lower, "_secondary_roads.shp"))
  # st_write(green_layer, paste0(main_dir, "/Cache/", place_name_lower, "_green_space.shp"))
  # st_write(city_border, paste0(main_dir, "/Cache/", place_name_lower, "_border.shp"))
  # st_write(clusters_10, paste0(main_dir, "/Cache/", place_name_lower, "_regions.shp"))
  # st_write(synthetic_sample_points, paste0(main_dir, "/Cache/", place_name_lower, "_points.shp"))
  
  # Tables ------------------------------------------------------------------
  
  # Generate dataframe for table for all 100 points
  sample_table <- synthetic_sample_points %>% st_drop_geometry() %>%
    select(region_loc, id, race, median_household_income_noise) %>% arrange(region_loc, id) %>%
    mutate(median_household_income_noise = paste0('$',comma(median_household_income_noise, accuracy = 1L)) 
    ) %>%
    mutate(race = case_when(race == 'Asian/Pacific Islander' ~ 'Asian',
                            race == 'Multiracial/Other' ~ 'Multi.',
                            race == 'Native American' ~ 'Native',
                            TRUE ~ as.character(race)
    ) ) %>%
    rename(`Region` = region_loc,
           `ID` = id,
           `Race/\nethnicity` = race, 
           `Household\nincome` = median_household_income_noise) 
  
  # Generate 10 individual tables by region
  r1 <- sample_table %>% filter(`Region` == 1) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r2 <- sample_table %>% filter(`Region` == 2) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r3 <- sample_table %>% filter(`Region` == 3) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r4 <- sample_table %>% filter(`Region` == 4) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r5 <- sample_table %>% filter(`Region` == 5) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r6 <- sample_table %>% filter(`Region` == 6) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r7 <- sample_table %>% filter(`Region` == 7) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r8 <- sample_table %>% filter(`Region` == 8) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r9 <- sample_table %>% filter(`Region` == 9) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  r10 <- sample_table %>% filter(`Region` == 10) %>% select(`ID`, `Race/\nethnicity`, `Household\nincome`) 
  
  # Combine tables into two rows
  (table_1_5 <- (ggplot() + labs(subtitle = 'Region 1') + ggplot2::annotate( geom = "table", x=0, y = 0, label = list(r1), 
                                                                             size = 2) + theme_void() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) +
      (ggplot() + labs(subtitle = 'Region 2') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r2), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 3') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r3), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 4') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r4), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 5') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r5), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      plot_layout(nrow = 1) )
  
  (table_6_10 <- (ggplot() + labs(subtitle = 'Region 6') + ggplot2::annotate( geom = "table", x=0, y = 0, label = list(r6), 
                                                                              size = 2) + theme_void() + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) +
      (ggplot() + labs(subtitle = 'Region 7') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r7), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 8') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r8), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 9') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r9), 
                                                                  size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) + 
      (ggplot() + labs(subtitle = 'Region 10') + ggplot2::annotate(geom = "table", x=0, y = 0, label = list(r10), 
                                                                   size = 2) + theme_void()  + theme(plot.subtitle = element_text(face = 'bold', hjust = .5))) +
      plot_layout(nrow = 1) )
  
  # Region Map --------------------------------------------------------------
  
  if (i != "3651000" & i != "0644000") {
    (region_map <- ggplot() +
       geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
       geom_sf(data = secondary_roads_layer, color = '#fae7af', alpha = 1, linewidth = .4) +
       geom_sf(data = roads_layer, color = '#fae7af', alpha = 1, linewidth = .6) +
       geom_sf_pattern(data = empty_tracts %>% st_union(), pattern = 'stripe', pattern_fill = '#eeeeee', pattern_colour = '#999999', alpha = 0.5, pattern_density = 0.5, pattern_angle = 45, pattern_spacing = 0.025) +
       geom_sf(data = city_border, color = '#999999', alpha = 0, linewidth = .6) +
       geom_sf(data = clusters_10, aes(fill = cluster_id), alpha = 0.2, linewidth = .6) + 
       geom_sf(data = bbox, alpha = 0, linewidth = 1) + 
       geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
       geom_text(data = clusters_10 %>% st_difference(., clusters_10 %>% st_boundary() %>% st_buffer(500) %>% st_simplify()) %>% filter(c(cluster_id == cluster_id.1)) %>% 
                   mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
                          lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])),
                 aes(x = lon, y = lat, label = region_loc), 
                 fontface = 'bold',
                 size = 6) + 
       labs(subtitle = "Regions",
            caption = "If there are greyed-out areas on the map, they usually represent unpopulated areas such as parks, universities or airports.") + 
       theme_void() + theme(panel.grid = element_blank(),
                            plot.subtitle = element_text(size = 15, face = 'bold', hjust = .5),
                            plot.caption = element_text(size = 10, hjust = 0.5, vjust = 0.5),
                            panel.border = element_blank(),
                            panel.background = element_blank(),
                            plot.margin=unit(c(t=0,r=0,b=25,l=0), "pt"),
                            legend.position = 'none'
       )
    )
  } else {
    (region_map <- ggplot() +
       geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
       geom_sf(data = secondary_roads_layer, color = '#fae7af', alpha = 1, linewidth = .4) +
       geom_sf(data = roads_layer, color = '#fae7af', alpha = 1, linewidth = .6) +
       
       geom_sf(data = empty_tracts %>% st_union(), fill = '#eeeeee', alpha = 0.5) + 
       # geom_sf_pattern(data = empty_tracts %>% st_union(), pattern = 'stripe', pattern_fill = '#eeeeee', pattern_colour = '#999999', alpha = 0.5, pattern_density = 0.5, pattern_angle = 45, pattern_spacing = 0.025) +
       geom_sf(data = city_border, color = '#999999', alpha = 0, linewidth = .6) +
       geom_sf(data = clusters_10, aes(fill = cluster_id), alpha = 0.2, linewidth = .6) + 
       geom_sf(data = bbox, alpha = 0, linewidth = 1) + 
       geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
       geom_text(data = clusters_10 %>% st_difference(., clusters_10 %>% st_boundary() %>% st_buffer(500) %>% st_simplify()) %>% filter(c(cluster_id == cluster_id.1)) %>% 
                   mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
                          lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])),
                 aes(x = lon, y = lat, label = region_loc), 
                 fontface = 'bold',
                 size = 6) + 
       labs(subtitle = "Regions",
            caption = "If there are greyed-out areas on the map, they usually represent unpopulated areas such as parks, universities or airports.") + 
       theme_void() + theme(panel.grid = element_blank(),
                            plot.subtitle = element_text(size = 15, face = 'bold', hjust = .5),
                            plot.caption = element_text(size = 10, hjust = 0.5, vjust = 0.5),
                            panel.border = element_blank(),
                            panel.background = element_blank(),
                            plot.margin=unit(c(t=0,r=0,b=25,l=0), "pt"),
                            legend.position = 'none'
       )
    )
  }
  
  design2 <- "
    AAAAAAA
    AAAAAAA
    AAAAAAA
    CCCCCCC
    DDDDDDD
  "
  
  # Tables/Region Map -------------------------------------------------------
  
  (regions_and_table <- region_map + table_1_5 + table_6_10 + 
    plot_layout(design = design2, guides = "collect"))

  ggsave(plot = regions_and_table,
         filename = paste0(wd,'/Plots/', place_name_lower, '_table', '.pdf'),
         width = 8.5, height = 11) # dpi = 300,

  # Teacher's Key -----------------------------------------------------------
  
  num_students <- 20
  runs <- 10
  fail <- FALSE
  
  # Run the sampling functions [num_students] times and check if the samples pass the QC check;
  # if not, repeat [runs] times, and if it fails every time, add it to qc_fails
  for (j in 1:runs) {
    seed <- j
    # n students' samples concatenated into one dataframe and id'ed by student
    judgment_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
      judgment(synthetic_sample_points, x * runs + seed) %>% mutate(student_id = x)
    }) %>% st_drop_geometry()
    
    simple_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
      simple_rs(synthetic_sample_points, x * runs + seed) %>% mutate(student_id = x)
    }) %>% st_drop_geometry()
    
    stratified_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
      stratified_rs(synthetic_sample_points, x * runs + seed) %>% mutate(student_id = x)
    }) %>% st_drop_geometry()
    
    cluster_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
      cluster_rs(synthetic_sample_points, x * runs + seed) %>% mutate(student_id = x)
    }) %>% st_drop_geometry()
    
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
  
  if (fail == TRUE) {
    warning(sprintf("%s did not pass the QC check", place_name))
    qc_fails <- append(qc_fails, i)
    # next
  } else {
    qc_passes <- append(qc_passes, i)
  }
    
  # Plots of sampled points (aggregated by student)
  
  bin_width <- 5000 # Should be a multiple of 1000
  
  # Min and max of x-axis on graphs are that city's min/max incomes in the 100-person population rounded up/down to the nearest [bin_width]
  lo <- floor(min(synthetic_sample_points$median_household_income_noise)/bin_width) * bin_width
  hi <- ceiling(max(synthetic_sample_points$median_household_income_noise)/bin_width) * bin_width
  
  # Only label every other tick on the plot (WARNING: NOT PARAMETERIZED TO bin_width)
  label_amounts <- seq(lo/1000, hi/1000, by = bin_width/1000)
  labels <- case_when(label_amounts %% 10 == 0 ~ paste0("$", label_amounts, "K"),
                      label_amounts %% 10 == 5 ~ "")

  judgment_hist <- ggplot(judgment_samples %>% # Calculate and bin medians of student samples
                            group_by(student_id) %>%
                            summarize(median_income = median(median_household_income_noise)) %>%
                            mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                          median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                          median_income >= hi ~ hi + bin_width)) %>%
                            group_by(income_bin) %>%
                            mutate(student_order = row_number()),
                          aes(y=student_order, x=income_bin)) + 
    geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise),
               color = '#4472C4', alpha = 0.7,
               size = 2) +
    geom_vline(xintercept = (mean(judgment_samples %>%
                                    group_by(student_id) %>%
                                    summarize(median_income = median(median_household_income_noise)) %>%
                                    ungroup() %>%
                                    pull())),
               color = '#F5870C', alpha = 0.7,
               size = 2) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
                       scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 9) + # Plot students as X's on a pseudo-histogram
    labs(title = place_name, 
         x = "Judgment Sample", 
         y = 'Student count') + 
    theme_classic() + 
    theme(plot.title = element_text(face = 'bold', hjust = 0.5),
          axis.text.x=element_text(size=6),
          axis.text.y=element_blank(), 
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank()) + 
    ggplot2::annotate("text",
                      x = hi, y = 10,
                      label = paste0("True median of 100 households ($", synthetic_sample %>% 
                                                                            select(median_household_income_noise) %>% pull() %>% median() %>% 
                                                                            round(.) %>% scales::label_comma(accuracy = 1)(.), ")"),
                      color = "#4472C4", fontface = "bold", hjust = 1) +
    ggplot2::annotate("text",
                      x = hi, y = 9,
                      label = "Mean of medians of students' samples",
                      color = "#F5870C", fontface = "bold", hjust = 1)

  simple_hist <- ggplot(simple_samples %>%
                              group_by(student_id) %>%
                              summarize(median_income = median(median_household_income_noise)) %>%
                              mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                            median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                            median_income >= hi ~ hi + bin_width)) %>%
                              group_by(income_bin) %>%
                              mutate(student_order = row_number()), 
                            aes(y=student_order, x=income_bin)) + 
    geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise),
               color = '#4472C4', alpha = 0.7,
               size = 2) +
    geom_vline(xintercept = (mean(simple_samples %>%
                                    group_by(student_id) %>%
                                    summarize(median_income = median(median_household_income_noise)) %>%
                                    ungroup() %>%
                                    pull())),
               color = '#F5870C', alpha = 0.7,
               size = 2) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 9) + 
    labs(x = "Simple Random Sample (SRS)", 
         y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(), 
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())
  
  stratified_hist <- ggplot(stratified_samples %>%
           group_by(student_id) %>%
           summarize(median_income = median(median_household_income_noise)) %>%
           mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                         median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                         median_income >= hi ~ hi + bin_width)) %>%
           group_by(income_bin) %>%
           mutate(student_order = row_number()), 
         aes(y=student_order, x=income_bin)) + 
    geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise),
               color = '#4472C4', alpha = 0.7,
               size = 2) +
    geom_vline(xintercept = (mean(stratified_samples %>%
                             group_by(student_id) %>%
                             summarize(median_income = median(median_household_income_noise)) %>%
                             ungroup() %>%
                             pull())),
               color = '#F5870C', alpha = 0.7,
               size = 2) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 9) + 
    labs(x = "Stratified Random Sample", 
         y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(), 
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank()) 
  
  cluster_hist <- ggplot(cluster_samples %>%
                              group_by(student_id) %>%
                              summarize(median_income = median(median_household_income_noise)) %>%
                              mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                            median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                            median_income >= hi ~ hi + bin_width)) %>%
                              group_by(income_bin) %>%
                              mutate(student_order = row_number()), 
                            aes(y=student_order, x=income_bin)) + 
    geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise),
               color = '#4472C4', alpha = 0.7,
               size = 2) +
    geom_vline(xintercept = (mean(cluster_samples %>%
                                    group_by(student_id) %>%
                                    summarize(median_income = median(median_household_income_noise)) %>%
                                    ungroup() %>%
                                    pull())),
               color = '#F5870C', alpha = 0.7,
               size = 2) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 9) + 
    labs(x = "Cluster Random Sample", 
         y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(), 
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  (key <- judgment_hist / simple_hist / cluster_hist / stratified_hist)
  
  ggsave(plot = key,
         filename = paste0(wd,'/Plots/', place_name_lower, '_key', '.pdf'),
         width = 8.5, height = 11) # dpi = 300,
  
  # Blank key  -------------------------------------------------------------
  
  blank_judgment_hist <- ggplot() +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
    labs(title = "Sampling Distributions for Median Household Income",
         x = "Judgment Sample",
         y = 'Student count') +
    theme_classic() +
    theme(plot.title = element_text(face = 'bold', hjust = 0.5),
          axis.text.x=element_text(size=6),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  blank_simple_hist <- ggplot() +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
    labs(x = "Simple Random Sample (SRS)",
         y = 'Student count') +
    theme_classic() +
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  blank_stratified_hist <- ggplot() +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
    labs(x = "Stratified Random Sample",
         y = 'Student count') +
    theme_classic() +
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  blank_cluster_hist <- ggplot() +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = labels) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 10)) +
    labs(x = "Cluster Random Sample",
         y = 'Student count') +
    theme_classic() +
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_blank(),
          axis.title.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  (blank_key <- blank_judgment_hist / blank_simple_hist / blank_cluster_hist / blank_stratified_hist)

  ggsave(plot = blank_key,
         filename = paste0(wd,'/Plots/', place_name_lower, '_blank_key', '.pdf'),
         width = 8.5, height = 11) # dpi = 300,
  
  # PDF Combining -----------------------------------------------------------
  
  qpdf::pdf_combine(input = c(paste0(wd,'/Plots/', place_name_lower, '_map', '.pdf'), 
                              paste0(wd,'/Plots/', place_name_lower, '_table', '.pdf'),
                              paste0(wd,'/Plots/', place_name_lower, '_blank_key', '.pdf')),
                    output = paste0(wd,'/Student/', place_name_lower, '_student_version', '.pdf'))
    
  qpdf::pdf_combine(input = c(paste0(wd,'/Plots/', place_name_lower, '_map', '.pdf'), 
                              paste0(wd,'/Plots/', place_name_lower, '_table', '.pdf'),
                              paste0(wd,'/Plots/', place_name_lower, '_key', '.pdf')),
                    output = paste0(wd,'/Teacher/', place_name_lower, '_teacher_version', '.pdf'))
  
} # End of loop (uncomment if looping)

# Appendix ------------------------------------------------------------------

# judgment_hist <- ggplot() + 
#   geom_histogram(data = judgment_samples %>% 
#                    group_by(student_id) %>% 
#                    summarize(median_income = median(median_household_income_noise)) %>%
#                    select(median_income),
#                  color = 'black',
#                  fill = 'white',
#                  aes(x = median_income),
#                  binwidth = hist_bin_width) + 
#   geom_density(data = synthetic_sample_points, 
#                aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
#   geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise), 
#              color = '#4472C4', alpha = 0.7,
#              size = 2) +
#   geom_vline(xintercept = (mean(judgment_samples %>% 
#                                   group_by(student_id) %>% 
#                                   summarize(median_income = median(median_household_income_noise)) %>%
#                                   ungroup() %>%
#                                   pull())), 
#              color = '#F5870C', alpha = 0.7,
#              size = 2) +
#   scale_x_continuous(name = 'Household income', 
#                      labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
#   expand_limits(y = c(0, 5)) + 
#   theme_classic() + 
#   labs(x = 'Median income', y = 'Student count') + 
#   ggtitle("Judgment Sample") + 
#   ggplot2::annotate("text", 
#                     x = max(synthetic_sample_points$median_household_income_noise), y = 4,
#                     label = "Median income of 100 households",
#                     color = "#4472C4", fontface = "bold", hjust = 1) + 
#   ggplot2::annotate("text",
#                     x = max(synthetic_sample_points$median_household_income_noise), y = 5,
#                     label = "Mean of medians of students' samples",
#                     color = "#F5870C", fontface = "bold", hjust = 1)
# 
# simple_hist <- ggplot() + 
#   geom_histogram(data = simple_samples %>% 
#                    group_by(student_id) %>% 
#                    summarize(median_income = median(median_household_income_noise)) %>%
#                    select(median_income),
#                  color = 'black',
#                  fill = 'white',
#                  aes(x = median_income),
#                  binwidth = hist_bin_width) + 
#   geom_density(data = synthetic_sample_points, 
#                aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
#   geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise), 
#              color = '#4472C4', alpha = 0.7,
#              size = 2) +
#   geom_vline(xintercept = (mean(simple_samples %>% 
#                                   group_by(student_id) %>% 
#                                   summarize(median_income = median(median_household_income_noise)) %>%
#                                   ungroup() %>%
#                                   pull())), 
#              color = '#F5870C', alpha = 0.7,
#              size = 2) +
#   scale_x_continuous(name = 'Household income', 
#                      labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
#   expand_limits(y = c(0, 5)) + 
#   theme_classic() + 
#   labs(x = 'Median income', y = 'Student count') + 
#   ggtitle("Simple Random Sample (SRS)")
# 
# stratified_hist <- ggplot() + 
#   geom_histogram(data = stratified_samples %>% 
#                    group_by(student_id) %>% 
#                    summarize(median_income = median(median_household_income_noise)) %>%
#                    select(median_income),
#                  color = 'black',
#                  fill = 'white',
#                  aes(x = median_income),
#                  binwidth = hist_bin_width) + 
#   geom_density(data = synthetic_sample_points, 
#                aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
#   geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise), 
#              color = '#4472C4', alpha = 0.7,
#              size = 2) +
#   geom_vline(xintercept = (mean(stratified_samples %>% 
#                                   group_by(student_id) %>% 
#                                   summarize(median_income = median(median_household_income_noise)) %>%
#                                   ungroup() %>%
#                                   pull())), 
#              color = '#F5870C', alpha = 0.7,
#              size = 2) +
#   scale_x_continuous(name = 'Household income', 
#                      labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
#   expand_limits(y = c(0, 5)) + 
#   theme_classic() + 
#   labs(x = 'Median income', y = 'Student count') + 
#   ggtitle("Stratified Random Sample")
# 
# cluster_hist <- ggplot() + 
#   geom_histogram(data = cluster_samples %>% 
#                    group_by(student_id) %>% 
#                    summarize(median_income = median(median_household_income_noise)) %>%
#                    select(median_income),
#                  color = 'black',
#                  fill = 'white',
#                  aes(x = median_income),
#                  binwidth = hist_bin_width) + 
#   geom_density(data = synthetic_sample_points, 
#                aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
#   geom_vline(xintercept = median(synthetic_sample_points$median_household_income_noise), 
#              color = '#4472C4', alpha = 0.7,
#              size = 2) +
#   geom_vline(xintercept = (mean(cluster_samples %>% 
#                                   group_by(student_id) %>% 
#                                   summarize(median_income = median(median_household_income_noise)) %>%
#                                   ungroup() %>%
#                                   pull())), 
#              color = '#F5870C', alpha = 0.7,
#              size = 2) +
#   scale_x_continuous(name = 'Household income', 
#                      labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
#   expand_limits(y = c(0, 5)) + 
#   theme_classic() + 
#   labs(x = 'Median income', y = 'Student count') + 
#   ggtitle("Cluster Random Sample")

# Old check function
# check <- function(pop, n_students = 20) {
#   # Generate samples
#   # Aggregate samples
#   simple_medians <- map_dfr(.x = seq(1, n_students), .f = function(x) {
#     simple_rs(pop) %>% mutate(student_id = x)
#   }) %>% 
#     group_by(student_id) %>% 
#     summarize(median = median(median_household_income_noise)) %>%
#     ungroup() %>%
#     select(median)
#   
#   stratified_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
#     stratified_rs(pop) %>% mutate(student_id = x)
#   }) %>% 
#     group_by(student_id) %>% 
#     summarize(median = median(median_household_income_noise)) %>%
#     ungroup() %>%
#     select(median)
#   
#   cluster_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
#     cluster_rs(pop) %>% mutate(student_id = x)
#   }) %>% 
#     group_by(student_id) %>% 
#     summarize(median = median(median_household_income_noise)) %>%
#     ungroup() %>%
#     select(median)
#   
#   # Calculate variances and check rank
#   simple_var <- var(simple_medians$median)
#   stratified_var <- var(stratified_medians$median)
#   cluster_var <- var(cluster_medians$median)
#   # simple_var <- var(simple_medians)
#   # stratified_var <- var(stratified_medians)
#   # cluster_var <- var(cluster_medians)
#   
#   if (stratified_var > simple_var | simple_var > cluster_var) {
#     return(FALSE)
#   }
#   
#   # Check bias
#   true_median <- median(pop$median_household_income_noise)
#   all_medians <- bind_rows(simple_medians, stratified_medians, cluster_medians)
#   lo <- min(all_medians$median)
#   hi <- max(all_medians$median)
#   distance <- max(abs(lo - true_median), abs(hi - true_median))
#   return(abs(true_median - mean(simple_medians$median))/distance <= 0.25 &
#            abs(true_median - mean(stratified_medians$median))/distance <= 0.25 &
#            abs(true_median - mean(cluster_medians$median))/distance <= 0.25)
# }
