
# data
library(dplyr)
library(tidyverse)
library(sf)
library(nngeo)
# census
library(tidycensus)
library(tigris)
# viz
library(patchwork)
library(scales)
library(viridis)
library(gt)
# utilities
library(janitor)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
options(tigris_use_cache = TRUE)


#install.packages('gtExtras')
set.seed(seed = 100)

# -------------------------------------------------------------------------

# census_api_key("YOUR KEY GOES HERE", install = TRUE)
readRenviron("~/.Renviron") # create an .Renviron > cd > touch .Renviron 
# Check ACS attributes for 2020 and 5 year estimates
census_data_dict <- load_variables(year = 2020, dataset = c('acs5'), cache = FALSE)


# Download Place data -----------------------------------------------------

# Read in Census Places (cities) population data
places_pop <- read_csv('https://www2.census.gov/programs-surveys/popest/datasets/2020-2022/cities/totals/sub-est2022.csv')

# Filter to largest 100 Places
places_pop_rank <- places_pop %>%
  rename_all(tolower) %>% 
  filter(sumlev %in% c('162')) %>%
  select(state, place, name, sumlev, stname, popestimate2020, popestimate2021, popestimate2022 ) %>%
  mutate(state = str_pad(state, width=2, side="left", pad="0"),
         place = str_pad(place, width=5, side="left", pad="0"),
         placeid = paste0(state,place)) %>%
  rename(cityname = name) %>%
  mutate(city_rank = row_number(desc(popestimate2020))) %>%
  filter(city_rank <= 100)

# Download Census Place delineations for a state
state_list = c('48') #
# state_list = c('17') # PARAMETERIZE

places_geo <- places(state = state_list, cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  inner_join(., places_pop_rank, by = c('GEOID'='placeid')) 

# Download data for clusters ----------------------------------------------

# Download data, recode variables to race / ethnicity categories, then aggregate
tract_data_race <- get_acs(year = 2020, geography = "tract", 
                           survey = 'acs5', variables = c('B03002_012', 'B03002_003', 'B03002_004', 'B03002_005', 'B03002_006', 'B03002_007', 'B03002_008', 'B03002_009'),
                           summary_var = 'B03002_001', 
                           cache_table = TRUE, 
                           state = '48', county = '029',
                           #state = '17', county = '031',  # PARAMETERIZE
                           geometry = FALSE) %>% 
  rename_all(list(tolower)) %>%
  mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino/a',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Native American',
                                    variable == 'B03002_006' ~ 'Asian / Pacific Islander',
                                    variable == 'B03002_007' ~ 'Asian / Pacific Islander',
                                    variable == 'B03002_008' ~ 'Multiracial / Other',
                                    variable == 'B03002_009' ~ 'Multiracial / Other',
                                    TRUE ~ as.character(''))) %>%
  group_by(geoid, variable_label, summary_est) %>% 
  summarize_at(.vars = vars(estimate), .funs = list(sum)) %>%
  ungroup() %>%
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

# Download tract geometries
tract_data_geo <- get_acs(year = 2020, geography = "tract", 
                          survey = 'acs5', variables = c('B19013_001'),
                          cache_table = TRUE, 
                          state = '48', county = '029',
                          #state = '17', county = '031',   # PARAMETERIZE
                          geometry = TRUE) %>% 
  rename_all(list(tolower))  %>%
  select(geoid, estimate, geometry) %>%
  rename(total_population = estimate)

# Join data and geometries
tract_data <- tract_data_geo %>%
  left_join(., tract_data_race, by = c('geoid' = 'geoid')) %>%
  filter(!is.na(plurality_race))  %>%
  st_transform(4326) %>%
  st_join(., places_geo %>% st_transform(4326), left = FALSE)

# Build spatial clusters --------------------------------------------------

# Dissolve tract geometries by plurality race
tract_data_grouped <- tract_data %>%
  group_by(plurality_race) %>% 
  dplyr::summarize(geometry = st_union(geometry)) %>%
  ungroup() %>% 
  st_cast(., "POLYGON") %>%
  st_transform(3395) %>%
  mutate(area = st_area(.)) %>%
  st_transform(4326) %>%
  group_by(plurality_race) %>%
  mutate(area_rank = row_number(desc(area))) %>%
  ungroup() %>%
  mutate(area_share = as.numeric(area/sum(area)))%>%
  filter(area_rank <= 1 | area_share >= .01) %>% # must have area over 1% or 1 cluster per race
  select(plurality_race, geometry) 

# ggplot() +
#   geom_sf(data = tract_data_grouped, aes(fill = plurality_race))

# Join dissolved clusters to tract data so every tract is assigned to its closest cluster
tract_data_clusters <- tract_data %>% st_make_valid() %>%
  select(geoid, total_population, geometry) %>%
  st_join(x = ., y = tract_data_grouped %>% st_make_valid(), left = TRUE, largest = TRUE) %>% 
  st_join(x = ., y = tract_data_grouped %>% st_make_valid(), join = st_nn)  %>%
  mutate(plurality_race = coalesce(plurality_race.x, plurality_race.y))

# ggplot() +
#   geom_sf(data = tract_data_clusters, aes(fill = plurality_race))

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

# ggplot() +
#   geom_sf(data = tract_data_clusters_grouped, aes(fill = plurality_race), color = 'white')

# Re-join dissolved clusters to tract data so every tract is assigned to its closest cluster
tract_data_clusters <- tract_data %>% st_make_valid() %>%
  select(geoid, total_population, geometry) %>%
  st_join(x = ., y = tract_data_clusters_grouped %>% st_make_valid(), left = TRUE, largest = TRUE) %>% 
  st_join(x = ., y = tract_data_clusters_grouped %>% st_make_valid(), join = st_nn)  %>%
  mutate(cluster_plurality_race = coalesce(plurality_race.x, plurality_race.y),
         cluster_group = coalesce(cluster_group.x, cluster_group.y)) %>%
  select(geoid, total_population, cluster_plurality_race, cluster_group, geometry) %>%
  st_transform(4326)

# ggplot() +
#    geom_sf(data = tract_data_clusters, aes(fill = cluster_plurality_race))


# Geometries --------------------------------------------------------------

# Download block group geometries
bg_data_geo <- get_acs(year = 2020, geography = "block group", 
                       survey = 'acs5', variables = c('B19013_001'),
                       cache_table = TRUE, 
                       state = '48', county = '029',
                       #state = '17', county = '031',  # PARAMETERIZE
                       geometry = TRUE) %>% 
  rename_all(list(tolower)) 

# Race / ethncity ---------------------------------------------------------

# Download data, recode variables to race / ethnicity categories, then aggregate
bg_data_race <- get_acs(year = 2020, geography = "block group", 
                        survey = 'acs5', variables = c('B03002_012', 'B03002_003', 'B03002_004', 'B03002_005', 'B03002_006', 'B03002_007', 'B03002_008', 'B03002_009'),
                        summary_var = 'B03002_001', 
                        cache_table = TRUE, 
                        state = '48', county = '029',
                        #state = '17', county = '031',  # PARAMETERIZE
                        geometry = FALSE) %>% 
  rename_all(list(tolower)) %>%
  mutate(tractid = str_sub(geoid, start = 1, end =  11)) %>%
  mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino/a',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Native American',
                                    variable == 'B03002_006' ~ 'Asian / Pacific Islander',
                                    variable == 'B03002_007' ~ 'Asian / Pacific Islander',
                                    variable == 'B03002_008' ~ 'Multiracial / Other',
                                    variable == 'B03002_009' ~ 'Multiracial / Other',
                                    TRUE ~ as.character(''))) %>%
  group_by(geoid, tractid, variable_label, summary_est) %>% 
  summarize_at(.vars = vars(estimate), .funs = list(sum)) %>%
  ungroup() 

# Calculate summary statistics
bg_data_race <- bg_data_race %>%
  mutate(race_share = estimate/summary_est) %>%
  group_by(tractid) %>%
  mutate(tract_estimate = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(tractid, variable_label) %>%
  mutate(tract_race_estimate = sum(estimate, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(tract_race_share = tract_race_estimate/tract_estimate,
         race_share = coalesce(race_share, tract_race_share)) %>%
  group_by(geoid) %>%
  mutate(plurality_rank = row_number(desc(race_share))) %>%
  ungroup() %>%
  filter(estimate > 0) %>%
  select(geoid, tractid, variable_label, estimate, race_share, plurality_rank) %>%
  rename(race_population = estimate,
         race = variable_label)

# Create dataframe of plurality race for each block group
bg_data_race_rank <- bg_data_race %>%
  filter(plurality_rank == 1) %>%
  rename(plurality_race_population = race_population, 
         plurality_race_share = race_share,
         plurality_race = race) %>%
  select(geoid, plurality_race, plurality_race_population, plurality_race_share)

# Create wide dataframe of race shares for each block group
bg_data_race_wide <-  bg_data_race %>%
  pivot_wider(id_cols = c(geoid),
              names_from = c(race),
              values_from = c(race_share, race_population),
              values_fill = 0) %>%
  rename_all(list(tolower)) %>%
  select_all(~gsub("\\s+|\\.|\\/", "_", .)) %>%
  select(geoid, race_share_asian___pacific_islander, race_share_black, race_share_latino_a, race_share_white, race_share_multiracial___other, race_share_native_american, 
         race_population_asian___pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial___other, race_population_native_american)

# Median Household Income -------------------------------------------------

# Download data for block groups
bg_data_income <- get_acs(year = 2020, geography = "block group", 
                          survey = 'acs5', variables = c('B19013_001'),
                          summary_var = 'B01003_001',
                          cache_table = TRUE,
                          state = '48', county = '029',
                          #state = '17', county = '031',  # PARAMETERIZE
                          geometry = FALSE) %>%
  mutate(variable_label = case_when(variable == 'B19013_001' ~ 'Median household income in the past 12 months')) %>%
  rename_all(list(tolower)) %>%
  select(geoid, estimate, summary_est) %>%
  mutate(tractid = str_sub(geoid, start = 1, end =  11)) %>%
  rename(total_population = summary_est,
         median_household_income = estimate) 

# Download data for tracts
tract_data_income <- get_acs(year = 2020, geography = "tract", 
                             survey = 'acs5', variables = c('B19013_001'),
                             cache_table = TRUE,
                             state = '48', county = '029',
                             #state = '17', county = '031',  # PARAMETERIZE
                             geometry = FALSE) %>%
  rename_all(list(tolower)) %>%
  rename(median_household_income_tract = estimate) %>%
  select(geoid, median_household_income_tract) 

# Impute missing block groups with tract data
bg_data_income <- bg_data_income %>%
  left_join(., tract_data_income,  by = c('tractid'='geoid')) %>%
  mutate(median_household_income = coalesce(median_household_income, 
                                            median_household_income_tract)) %>%
  select(geoid, total_population, median_household_income) %>%
  mutate(household_income_bucket = case_when(median_household_income < 25000 ~ '1 - $0-25k',
                                             median_household_income >= 25000 & median_household_income < 50000 ~ '2 - $25-50k',
                                             median_household_income >= 50000 & median_household_income < 75000 ~ '3 - $50-75k',
                                             median_household_income >= 75000 & median_household_income < 100000 ~ '4 - $75-100k',
                                             median_household_income >= 100000 & median_household_income < 150000 ~ '5 - $100-150k',
                                             median_household_income >= 150000 ~ '6 - $150k+'))

# Combine data ------------------------------------------------------------

# Join together block group data
bg_data <- bg_data_geo %>%
  left_join(., bg_data_race_wide, by = c('geoid'='geoid')) %>%
  left_join(., bg_data_race_rank, by = c('geoid'='geoid')) %>%
  left_join(., bg_data_income, by = c('geoid'='geoid')) %>%
  select( geoid, total_population, median_household_income, household_income_bucket, plurality_race, plurality_race_population, plurality_race_share, 
          race_share_asian___pacific_islander, race_share_black, race_share_latino_a, race_share_white, race_share_multiracial___other, race_share_native_american, 
          race_population_asian___pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial___other, race_population_native_american,
          geometry) %>%
  filter(total_population > 0) %>% st_transform(4326) %>%
  st_join(x = ., y = places_geo %>% st_transform(3857) %>% 
            st_buffer(100) %>% st_transform(4326), 
          join = st_within, left = FALSE)

# Filter out NAs
bg_data <- bg_data %>%
  filter(!is.na(median_household_income))

# -------------------------------------------------------------------------

# Race distribution of city
plurality_tract_race_list <- tract_data_clusters %>% st_drop_geometry() %>% 
  select(cluster_plurality_race) %>% distinct() %>% pull()

city_distribution <- bg_data %>% st_drop_geometry() %>%
  summarize_at(vars(race_population_asian___pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial___other, race_population_native_american), 
               list(sum)) %>%
  pivot_longer(cols =c("race_population_asian___pacific_islander", "race_population_black", "race_population_latino_a", "race_population_white", "race_population_multiracial___other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino_a" ~ 'Latino/a',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian___pacific_islander" ~ 'Asian / Pacific Islander',
                          race == "race_population_multiracial___other" ~ 'Multiracial / Other')) %>%
  filter(race %in% plurality_tract_race_list) %>% ## FILTERS OUT RACES WITHOUT A PLURALITY TRACT -- ONE FIX IS USE BLOCK GROUPS INSTEAD OF TRACTS FOR REGIONALIZATION
  mutate(share = value / sum(value), 
         below_10 = case_when(share < .1 ~ .1, TRUE ~ 0),
         realloc_factor = 1-sum(below_10)) %>%
  group_by(below_10) %>%
  mutate(share_realloc = (share/sum(share))*realloc_factor) %>%
  ungroup() %>%
  mutate(share_realloc = case_when(share < .1 ~ .1, TRUE ~ share_realloc),
         share_realloc = round(share_realloc, 1),
         cluster_count = 10*share_realloc, 
         cluster_count = 10*(cluster_count/sum(cluster_count))) %>% ## NORMALIZE COUNT TO 10
  uncount(cluster_count) %>%
  sample_n(size = 10) %>% # come up with way to force minority clusters to always be sampled
  group_by(race) %>%
  tally(name = 'cluster_count')
  
# Block groups data
bg_data_clusters <- bg_data %>% 
  st_make_valid() %>%
  st_transform(4236) %>%
  mutate(tractid = str_sub(geoid, start = 1, end =  11)) %>%
  left_join(., tract_data_clusters %>% st_drop_geometry() %>% 
              select(geoid, cluster_plurality_race, cluster_group),
            by = c('tractid' = 'geoid'))

ggplot() +
  geom_sf(data = bg_data_clusters, aes(fill = cluster_plurality_race))

sample_points <- purrr::map_dfr(
  .x = city_distribution %>% select(race) %>% pull(), 
  .f = function(i) {
    print(i)
    
    num_points <- city_distribution %>%
      filter(race == i) %>% 
      select(cluster_count) %>% pull() 
  
    # Population-weighted XY k-means
    k_clusters <- stats::kmeans(x = bg_data_clusters %>%
                                  filter(cluster_plurality_race == i) %>%
                                  #st_transform(3395) %>%
                                  st_centroid() %>%
                                  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]) ,
                                         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]) ) %>%
                                  st_drop_geometry() %>%
                                  mutate(lon = scale(lon, center = TRUE, scale = TRUE)*total_population,
                                         lat = scale(lat, center = TRUE, scale = TRUE)*total_population) %>%
                                  select(lon, lat) %>%
                                  as.matrix(),
                                centers = num_points)

    # XY-only k-means
    # k_clusters <- stats::kmeans(x = bg_data_clusters %>%
    #                               filter(cluster_plurality_race == i) %>%
    #                               st_centroid() %>% st_coordinates(),
    #                             centers = num_points)
    
    race_block_groups <- bg_data_clusters %>% 
      filter(cluster_plurality_race == i) %>%
      mutate(cluster_race = k_clusters$cluster) %>%
      st_make_valid() %>%
      group_by(cluster_race) %>% # cluster_group,
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
  geom_sf(data = bg_data_clusters, aes(fill = cluster_plurality_race)) +
  geom_sf(data = sample_points, color = 'black')

bg_data_sample <- sample_points %>% 
  st_transform(3395) %>%
  st_join(., bg_data %>% st_transform(3395), left = TRUE,
          join = st_nn, k = 10) %>% #, returnDist = TRUE
  st_transform(4326)# %>%
  #st_drop_geometry() %>%
  # group_by(cluster_id) %>%
  # sample_frac(size = .5, weight = total_population) %>%
  # ungroup() 

bg_data_all <- bg_data %>% st_transform(3395) %>%
  st_join(., sample_points %>% st_transform(3395), left = TRUE,
          join = st_nn, k = 1) %>% 
  st_transform(4326)
names(bg_data_all)

# Regional sample option
ggplot() +
  geom_sf(data = bg_data_all, aes(fill = cluster_id), color = 'white') +
  geom_sf(data = sample_points, color = 'black') +
  theme_void()


# -------------------------------------------------------------------------

# Clusters
clusters <- bg_data_clusters %>%
  select(cluster_plurality_race, total_population, geometry) %>%
  st_make_valid() %>%
  group_by(cluster_plurality_race) %>%
  dplyr::summarize(geometry = st_union(geometry), population = sum(total_population)) %>%
  ungroup() %>% 
  st_make_valid() %>%
  mutate(population_share = population/sum(population)) %>%
  st_cast(., "POLYGON") %>%
  st_transform(3395) %>%
  mutate(area_km2 = as.numeric(st_area(.))*1e-6) %>%
  st_transform(4326) %>%
  arrange(cluster_plurality_race, desc(area_km2)) %>%
  mutate(cluster_group = row_number()) %>%
  group_by(cluster_plurality_race) %>%
  mutate(cluster_count = sum(n())) %>%
  ungroup()

# Clusters by race
ggplot() +
  geom_sf(data = clusters, aes(fill = cluster_plurality_race), color = 'white')+ 
  theme_void()


# Roads and water ---------------------------------------------------------

roads_layer <- tigris::primary_roads(year = 2020) %>%
  st_transform(4326) %>% 
  st_intersection(., clusters)

water_layer <- tigris::area_water(state = '48', county = '029', year = 2020) %>%
  st_transform(4326) %>% 
  st_intersection(., clusters)

# -------------------------------------------------------------------------

# Create synthetic 100 households
cluster_aggregate_data <- bg_data_all %>%
  st_drop_geometry() %>%
  group_by(cluster_id_race, cluster_id, median_household_income) %>%
  summarize_at(vars(race_population_asian___pacific_islander, race_population_black, race_population_latino_a, race_population_white, race_population_multiracial___other, race_population_native_american), 
               list(sum)) %>%
  ungroup() %>%
  pivot_longer(cols =c("race_population_asian___pacific_islander", "race_population_black", "race_population_latino_a", "race_population_white", "race_population_multiracial___other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino_a" ~ 'Latino/a',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian___pacific_islander" ~ 'Asian / Pacific Islander',
                          race == "race_population_multiracial___other" ~ 'Multiracial / Other')) %>%
  mutate(median_household_income_wt = median_household_income * value) %>%
  group_by(cluster_id_race, cluster_id, race) %>%
  summarize_at(vars(median_household_income_wt, value),
               list(sum)) %>%
  ungroup() %>%
  mutate(median_household_income = median_household_income_wt/value) %>%
  select(-one_of(c('median_household_income_wt'))) %>%
  group_by(cluster_id) %>%
  mutate(share = value/sum(value),
         count = round(value/sum(value) * 110, 0)) %>%
  ungroup()
  
synthetic_distribution <- cluster_aggregate_data %>%
  #filter(count > 0) %>%
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

synthetic_sample <- synthetic_distribution %>%
  filter(rank_random <= 10) %>%
  group_by(cluster_id, race) %>%
  mutate(freq_race = sum(n())) %>%
  ungroup() %>%
  group_by(cluster_id) %>%
  mutate(synth_id = row_number(desc(freq_race))) %>%
  ungroup() %>%
  select(cluster_id_race, cluster_id, synth_id, race, median_household_income_noise)

(missing_races <- setdiff(bg_data_race %>% select(race) %>% distinct() %>% pull(),
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
    print('Race / ethnicity absent from synthetic count data') ## if this happens make it retry -- happens when there is a group with less than 1pct
  }
} else { 
  print('No missing race / ethnicities.') 
}

# Viz ---------------------------------------------------------------------

clusters_10 <- bg_data_all %>% 
  group_by(cluster_id) %>% # cluster_group,
  dplyr::summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  mutate(lon = map_dbl(geometry, ~st_centroid(.x)[[1]]) ,
         lat = map_dbl(geometry, ~st_centroid(.x)[[2]])) %>%
  mutate(lon_tile = ntile(-desc(lon), 5),
         lat_tile = ntile(desc(lat), 5)) %>%
  arrange(lat_tile, lon_tile, -desc(lon)) %>%
  mutate(region_loc = row_number())

clusters_padding <- clusters_10 %>%
  st_difference(., clusters_10 %>% 
                st_boundary() %>% st_as_sf() %>%
                st_transform(3395) %>%
                st_buffer(., dist = 900) %>%
                st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_make_valid() )

dots <- sf::st_sample(clusters_padding, size = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100), by_polygon = TRUE, type = 'regular') %>%
  st_as_sf() %>%
  st_join(., clusters_10, left = FALSE, largest = TRUE)

# ggplot() +
#   geom_sf(data = clusters_10, color = 'black', fill = 'white', alpha = 0, linewidth = .2) +
#   geom_sf(data = dots, color = 'black', fill = 'black', alpha = 1, size = 1, linewidth = .2) +
#   theme_void()

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


synthetic_sample_points <- dots2  %>% 
  left_join(., synthetic_sample, by = c('cluster_id' = 'cluster_id', 
                                        'synth_id' = 'synth_id')) %>%
  mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]) ,
         lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
  arrange(region_loc, -desc(lon)) %>%
  mutate(id = row_number()) 


color_vec <- c('#F5870C', '#4472C4', '#06c049', '#70309F', '#fece0a', '#FF0000')

(map <- ggplot() +
  geom_sf(data = water_layer, color = '#1b95e0', fill = '#1b95e0', alpha = .3, linewidth = .6) +
  geom_sf(data = clusters_10, fill = '#cfd0c5', alpha = .5, linewidth = .6) +
  geom_sf(data = roads_layer, color = 'white', alpha = 1, linewidth = .4) +
  geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
  geom_sf(data = synthetic_sample_points, 
          aes(color = race, fill = race, size = median_household_income_noise ),  alpha = .8, linewidth = .2) + #size = 6.5,
  ggrepel::geom_text_repel(data = synthetic_sample_points,
                           seed = 1, segment.curvature = 0, point.padding = 0, box.padding = 0, max.iter = 1000, segment.square  = FALSE, segment.inflect = FALSE, 
                           min.segment.length = 0, max.overlaps = Inf, force = .01, force_pull = 2, aes(x = lon, y = lat, label = id), 
                           size = 3, vjust =.5, color = 'white', fontface='bold') + 
  guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
  scale_fill_manual(values = color_vec, name = 'Race / ethnicity') +
  scale_color_manual(values = color_vec, name = 'Race / ethnicity' ) +
  scale_size_binned(name = 'Household income', range = c(3, 12),
                    labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
  guides(color = guide_legend(override.aes = list(size = 8, alpha = 1))) +
  theme_void() + theme(legend.position = 'right',
                       legend.title = element_text(size = 11, face = 'bold'),
                       legend.text = element_text(size = 11),
                       legend.margin=margin(0,0,0,0),
                       legend.box.margin=margin(0,0,0,0),
                       plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
                       legend.box = 'vertical'
                       ))


# -------------------------------------------------------------------------

donut_data <- cluster_aggregate_data %>% 
  left_join(., clusters_10 %>% st_drop_geometry() %>% 
              select(cluster_id, region_loc), by = c('cluster_id'='cluster_id')) %>%
  group_by(cluster_id) %>%
  mutate(race = factor(race, levels = c("Asian / Pacific Islander", "Black", "Latino/a", "Multiracial / Other", "Native American", "White")),
         region_median_household_income = round( (sum(median_household_income*value)/sum(value)) / 1000,0)
  ) %>%
  arrange(region_loc, race) %>%
  mutate(ymax = cumsum(share)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  ungroup() %>%
  mutate(region_label = reorder(paste0('Region ',region_loc,'\n$',comma(region_median_household_income),'K'), region_loc))

(donut_charts <- ggplot(donut_data, 
                        aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=race)) +
    geom_rect() +
    coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
    xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
    facet_wrap(~region_label, nrow = 2) +
    #facet_wrap(~region_label, ncol = 2) + 
    scale_fill_manual(values = color_vec, expand = c(0, 0)) +
    scale_color_manual(expand = c(0, 0)) + 
    theme_void() +
    theme(legend.position = 'none', 
          plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
          panel.border = element_blank(),
          panel.background = element_blank(),
          panel.grid = element_blank()))

 design <- "
  AAAAAAA
  AAAAAAA
  AAAAAAA
  BBBBBBB
"
 
#  design <- "
#   AAAAAAABB
#   AAAAAAABB
#   AAAAAAABB
#   AAAAAAABB
# "
 
(viz <- map / donut_charts +
   plot_layout(design = design))
 
 ggsave(plot = viz, filename = '/Users/nm/Desktop/Projects/work/skew-the-script/test.pdf', width = 8.5, height = 11) # dpi = 300,
 


# -------------------------------------------------------------------------

 
 sample_table <- synthetic_sample_points %>% st_drop_geometry() %>%
   select(region_loc, id, race, median_household_income_noise) %>% arrange(region_loc, id) %>%
   mutate(median_household_income_noise = paste0('$',comma(median_household_income_noise, accuracy = 1L)) 
   ) %>%
   rename(`Region` = region_loc,
          `Person ID` = id,
          `Race / ethnicity` = race, 
          `Household income` = median_household_income_noise) 
 
 r1 <- sample_table %>% filter(`Region` == 1) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r2 <- sample_table %>% filter(`Region` == 2) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r3 <- sample_table %>% filter(`Region` == 3) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r4 <- sample_table %>% filter(`Region` == 4) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r5 <- sample_table %>% filter(`Region` == 5) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r6 <- sample_table %>% filter(`Region` == 6) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r7 <- sample_table %>% filter(`Region` == 7) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r8 <- sample_table %>% filter(`Region` == 8) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r9 <- sample_table %>% filter(`Region` == 9) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 r10 <- sample_table %>% filter(`Region` == 10) %>% select(`Person ID`, `Race / ethnicity`, `Household income`) %>% gt() %>% tab_options(column_labels.hidden = TRUE, table.font.size = px(12L)) %>% as_raw_html()
 
 reg_tables_1 <- data.frame(r1 = r1, r2 = r2, r3 = r3, r4 = r4, r5 = r5)
 
 reg_tables_2 <- data.frame(r6 = r6, r7 = r7, r8 = r8, r9 = r9, r10 = r10)
 
 rt1 <- reg_tables_1 %>% 
   gt() %>% 
   fmt_markdown(columns = everything()) %>% 
   cols_label(r1 = 'Region 1', r2 = 'Region 2', r3 = 'Region 3', r4 = 'Region 4', r5 = 'Region 5')
 
 rt2 <- reg_tables_2 %>% 
   gt() %>% 
   fmt_markdown(columns = everything()) %>% 
   cols_label(r6 = 'Region 6', r7 = 'Region 7', r8 = 'Region 8', r9 = 'Region 9', r10 = 'Region 10')
 

# -------------------------------------------------------------------------

 
# -------------------------------------------------------------------------

 
 
 
 library(ggpmisc)
sample_table2 <-(ggplot() + 
    ggplot2::annotate(geom = "table", x=0, y = 0, label = list(sample_table %>% filter(`Person ID` <= 50) ), 
                      size = 2) + theme_void()) + 
   (ggplot() + 
      ggplot2::annotate(geom = "table", x=0, y = 0, label = list(sample_table %>% filter(`Person ID` > 50) ), 
                        size = 2) + theme_void()) 
 
 
# -------------------------------------------------------------------------

 # QC ----------------------------------------------------------------------
 
 # City totals
 control_qc <- bg_data %>%
   st_drop_geometry() %>%
   group_by(household_income_bucket) %>%
   summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
                list(sum)) %>%
   pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
                names_to = c("race")) %>%
   mutate(race = case_when(race == "race_population_latino" ~ 'Latino/a',
                           race == "race_population_white" ~ 'White',
                           race == "race_population_black" ~ 'Black',
                           race == "race_population_native_american" ~ 'Native American',
                           race == "race_population_asian_pacific_islander" ~ 'Asian / Pacific Islander',
                           race == "race_population_multiracial_other" ~ 'Multiracial / Other')) %>%
   group_by(household_income_bucket) %>%
   mutate(share = value/sum(value)) %>%
   ungroup() %>%
   rename(city_pop = value,
          city_shr = share)
 
 sample_qc_a <- bg_data_all %>%
   st_drop_geometry() %>%
   group_by(household_income_bucket) %>%
   summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
                list(sum)) %>%
   ungroup() %>%
   pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
                names_to = c("race")) %>%
   mutate(race = case_when(race == "race_population_latino" ~ 'Latino/a',
                           race == "race_population_white" ~ 'White',
                           race == "race_population_black" ~ 'Black',
                           race == "race_population_native_american" ~ 'Native American',
                           race == "race_population_asian_pacific_islander" ~ 'Asian / Pacific Islander',
                           race == "race_population_multiracial_other" ~ 'Multiracial / Other')) %>%
   group_by(household_income_bucket) %>%
   mutate(share = value/sum(value)) %>%
   ungroup()  %>%
   rename(samp_pop = value,
          samp_shr = share)
 
 
#QC
 
 
 # synthetic_sample <- synthetic_distribution %>% 
 #   group_by(cluster_id) %>%
 #   sample_n(10) %>%
 #   bind_rows(synthetic_min_quota) 
 
 # sample <- synthetic_distribution %>% 
 #   group_by(cluster_id) %>%
 #   sample_n(24 - nrow(sythetic_min_quota)) %>%  
 #   bind_rows(df2)  
 
 sample_qc_b <- synthetic_distribution %>%
   mutate(household_income_bucket = case_when(median_household_income_noise < 25000 ~ '1 - $0-25k',
                                              median_household_income_noise >= 25000 & median_household_income_noise < 50000 ~ '2 - $25-50k',
                                              median_household_income_noise >= 50000 & median_household_income_noise < 75000 ~ '3 - $50-75k',
                                              median_household_income_noise >= 75000 & median_household_income_noise < 100000 ~ '4 - $75-100k',
                                              median_household_income_noise >= 100000 & median_household_income_noise < 150000 ~ '5 - $100-150k',
                                              median_household_income_noise >= 150000 ~ '6 - $150k+'),
          count = 1) %>%
   group_by(race, household_income_bucket) %>%
   summarize_at(vars(count), list(sum)) %>%
   ungroup() %>%
   mutate(share = count/sum(count)) %>%
   ungroup()  %>%
   group_by(household_income_bucket) %>%
   mutate(share = count/sum(count)) %>%
   ungroup()  %>%
   rename(samp_pop = count,
          samp_shr = share)
 


# -------------------------------------------------------------------------


ggplot() +
  geom_sf(data = water_layer, color = '#1b95e0', fill = '#1b95e0', alpha = .3, linewidth = .6) +
  geom_sf(data = clusters_10, color = '#333333', fill = '#b8bdc0', alpha = .5, linewidth = .6) +
  geom_sf(data = roads_layer, color = 'white', fill = 'white', alpha = 1, linewidth = 1) +
  #geom_sf_text(data = landmark_layer, aes(label = str_wrap(FULLNAME,8), fontface = 'bold'), color = 'black', fill = 'black', size = 3, check_overlap = TRUE) +
  geom_sf(data = clusters_10, color = '#333333', fill = 'white', alpha = 0, linewidth = .6) +
  geom_sf(data = synthetic_sample_points, aes(color = race, fill = race), size = 6.5, alpha = .9, linewidth = .2) +
  geom_sf_text(data = synthetic_sample_points, aes(label = id), size = 4, color = 'white', fontface='bold' ) +
  scale_fill_manual(values = color_vec, name = 'Race / ethnicity') +
  scale_color_manual(values = color_vec, name = 'Race / ethnicity' ) +
  theme_void() + theme(legend.position = 'bottom') +
ggplot() +
  geom_sf(data = water_layer, color = '#1b95e0', fill = '#1b95e0', alpha = .3, linewidth = .6) +
  geom_sf(data = clusters_10, color = '#333333', fill = '#b8bdc0', alpha = .5, linewidth = .6) +
  geom_sf(data = roads_layer, color = 'white', fill = 'white', alpha = 1, linewidth = 1) +
  #geom_sf_text(data = landmark_layer, aes(label = str_wrap(FULLNAME,8), fontface = 'bold'), color = 'black', fill = 'black', size = 3, check_overlap = TRUE) +
  geom_sf(data = clusters_10, color = '#333333', fill = 'white', alpha = 0, linewidth = .6) +
  geom_sf(data = synthetic_sample_points, aes(color = median_household_income_noise), fill = 'white', size = 6.5, alpha = .9) +
  scale_color_distiller(name = 'Household income', direction = -1, palette = 'Spectral', oob = scales::squish,
                       labels = label_dollar(accuracy = 1L, scale =  0.001, prefix = '$', suffix = "K")) +
  # scale_color_viridis(name = 'Household income', direction = -1, oob = scales::squish, 
  #                       labels = label_dollar(accuracy = 1L, scale =  0.001, prefix = '$', suffix = "K")) +
  geom_sf_text(data = synthetic_sample_points, aes(label = id), size = 4, color = 'black', fontface='bold' ) +
  theme_void() + theme(legend.position = 'bottom',
                       legend.key.width=unit(40,"pt"),
                       legend.key.height=unit(10,"pt") )

# make lat lon order of clusters

# ggplot() +
#   geom_sf(data = clusters, aes(fill = cluster_plurality_race)) +
#   theme_void()

# -------------------------------------------------------------------------

ggplot(data = control_qc, aes(x = household_income_bucket, y = city_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") + 
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') + 
ggplot(data = sample_qc_a, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') +
ggplot(data = sample_qc_b, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') 


# QC Check ----------------------------------------------------------------

# Appendix ----------------------------------------------------------------

# looping starter code
places_list <- places(state = c('17','48') , cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  inner_join(., places_pop_rank, by = c('GEOID'='placeid')) %>%
  rename_all(tolower) %>%
  select(geoid, name, popestimate2020, popestimate2021, popestimate2022, city_rank, geometry)

state_xwalk <- tidycensus::fips_codes %>%
  mutate(county_fips = paste0(state_code,county_code))

us_county <- get_acs(year = 2020, geography = "county", variables = "B01003_001", 
                     geometry = TRUE, shift_geo = FALSE) %>%
  rename_all(tolower) %>% 
  select(geoid, estimate) %>%
  rename(county_fips = geoid) %>%
  left_join(., state_xwalk, by = c('county_fips' = 'county_fips'))

us_county <- us_county %>% 
  select(state, county_fips, state_code, county_code) %>%
  st_transform(4326) %>%
  st_join(., places_list %>% 
            st_transform(4326), left = FALSE)

## COME UP WITH METHOD TO BUILD A DICT OF PLACES AND STATE AND COUNTY AS KEYS

city_cty_dict <- us_county %>%
  select(geoid, name, state_code, county_code) %>%
  st_drop_geometry() 

for (i in unique(city_cty_dict$geoid)) {
  city_cty_dict %>% filter(geoid == i) %>%
    select(name) %>% pull() %>% unique() %>% print()
  city_list <- city_cty_dict %>% filter(geoid == i) %>%
    select(state_code, county_code) 
  state_list <- city_list%>% select(state_code) %>% pull()
  county_list <- city_list %>% select(county_code) %>% pull()
  
  acs_data <- map2_dfr(.x = state_list, .y = county_list, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", survey = 'acs5',
            variables = acs5_vars_selected,
            state = x, county = y)
  })

}

# z <- bg_data_clusters %>%
#   st_transform(3395) %>%
#   mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
#          lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])) %>%
#   st_drop_geometry() %>%
#   group_by(cluster_group) %>%
#   sample_frac(size = 10/(bg_data_clusters %>% 
#                            st_drop_geometry() %>% nrow()), 
#               weight = total_population) %>%
#   ungroup() %>%
#   st_as_sf(coords = c("lon", "lat"),
#            crs = 3395, agr = "constant")
  

# https://www.placeiq.com/2021/06/segmenting-the-us-with-observation-weighted-k-means-clustering/
