
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
# utilities
library(janitor)
library(conflicted)
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
options(tigris_use_cache = TRUE)

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
state_list = c('48') #c('17') # PARAMETERIZE

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
  mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Native American',
                                    variable == 'B03002_006' ~ 'Asian Pacific Islander',
                                    variable == 'B03002_007' ~ 'Asian Pacific Islander',
                                    variable == 'B03002_008' ~ 'Multiracial other',
                                    variable == 'B03002_009' ~ 'Multiracial other',
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
  mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Native American',
                                    variable == 'B03002_006' ~ 'Asian Pacific Islander',
                                    variable == 'B03002_007' ~ 'Asian Pacific Islander',
                                    variable == 'B03002_008' ~ 'Multiracial other',
                                    variable == 'B03002_009' ~ 'Multiracial other',
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
  select(geoid, race_share_white, race_share_black, race_share_latino, race_share_asian_pacific_islander, race_share_multiracial_other, race_share_native_american,
         race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american) 

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


# -------------------------------------------------------------------------
# Median household income by race (if we need it)
# variable == 'B19013B_001' ~ 'Black',
# variable == 'B19013C_001' ~ 'Native American',
# variable == 'B19013D_001' ~ 'Asian',
# variable == 'B19013E_001' ~ 'Pacific Islander',
# variable == 'B19013F_001' ~ 'Other race',
# variable == 'B19013G_001' ~ 'Multiracial',
# variable == 'B19013H_001' ~ 'White',
# variable == 'B19013I_001' ~ 'Latino',

# Combine data ------------------------------------------------------------

# Join together block group data
bg_data <- bg_data_geo %>%
  left_join(., bg_data_race_wide, by = c('geoid'='geoid')) %>%
  left_join(., bg_data_race_rank, by = c('geoid'='geoid')) %>%
  left_join(., bg_data_income, by = c('geoid'='geoid')) %>%
  select( geoid, total_population, median_household_income, household_income_bucket, plurality_race, plurality_race_population, plurality_race_share, 
          race_share_white, race_share_black, race_share_latino, race_share_asian_pacific_islander, race_share_multiracial_other, race_share_native_american, 
          race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american,
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
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american), 
               list(sum)) %>%
  pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
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
    
    k_clusters <- stats::kmeans(x = bg_data_clusters %>% 
                                  filter(cluster_plurality_race == i) %>% 
                                  st_centroid() %>% st_coordinates(), 
                                centers = num_points)
    
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
          join = st_nn, k = 10) %>% 
  st_transform(4326)# %>%
  #st_drop_geometry() %>%
  # group_by(cluster_id) %>%
  # sample_frac(size = .5, weight = total_population) %>%
  # ungroup() 

bg_data_100 <- bg_data_geo %>%
  st_transform(4326) %>%
  select(geoid) %>%
  inner_join(., bg_data_sample %>% 
              select(geoid, cluster_id_race, cluster_id, 
                     total_population, median_household_income, household_income_bucket,
                     plurality_race, plurality_race_population, plurality_race_share,
                     race_share_white, race_share_black, race_share_latino, race_share_asian_pacific_islander, race_share_multiracial_other, race_share_native_american, 
                     race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american) %>%
               st_drop_geometry(),
            by = c('geoid' = 'geoid')) 

names(bg_data_sample)


bg_data_all <- bg_data %>% st_transform(3395) %>%
  st_join(., sample_points %>% st_transform(3395), left = TRUE,
          join = st_nn, k = 1) %>% 
  st_transform(4326)
names(bg_data_all)

ggplot() +
  geom_sf(data = bg_data_all, aes(fill = cluster_id), color = 'white') +
  theme_void()


# QC ----------------------------------------------------------------------

# City totals
control_qc <- bg_data %>%
  st_drop_geometry() %>%
  group_by(household_income_bucket) %>%
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
               list(sum)) %>%
  pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
  group_by(household_income_bucket) %>%
  mutate(share = value/sum(value)) %>%
  ungroup() %>%
  rename(city_pop = value,
         city_shr = share)

sample_qc_1a <- bg_data_100 %>%
  st_drop_geometry() %>%
  group_by(household_income_bucket) %>%
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
               list(sum)) %>%
  ungroup() %>%
  pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
  group_by(household_income_bucket) %>%
  mutate(share = value/sum(value)) %>%
  ungroup()  %>%
  rename(samp_pop = value,
         samp_shr = share)

sample_qc_2a <- bg_data_all %>%
  st_drop_geometry() %>%
  group_by(household_income_bucket) %>%
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
               list(sum)) %>%
  ungroup() %>%
  pivot_longer(cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
  group_by(household_income_bucket) %>%
  mutate(share = value/sum(value)) %>%
  ungroup()  %>%
  rename(samp_pop = value,
         samp_shr = share)

# qc <- control_qc %>%
#   left_join(., sample_qc_1a , by = c('household_income_bucket'='household_income_bucket',
#                                   'race'='race')) %>%
#   mutate(difference = city_shr - samp_shr)

# -------------------------------------------------------------------------

# Synthetic generated individual data
cluster_distribution <- bg_data_100 %>%
  st_drop_geometry() %>%
  group_by(cluster_id_race, cluster_id, median_household_income) %>%
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
               list(sum)) %>%
  ungroup() %>%
  pivot_longer(
    cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
               names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
  mutate(median_household_income_wt = median_household_income * value) %>%
  group_by(cluster_id_race, cluster_id, race) %>%
  summarize_at(vars(median_household_income_wt, value),
               list(sum)) %>%
  ungroup() %>%
  mutate(median_household_income = median_household_income_wt/value) %>%
  select(-one_of(c('median_household_income_wt'))) %>%
  group_by(cluster_id) %>%
  mutate(share = value/sum(value),
         count = round(value/sum(value) * 20, 0)) %>%
  ungroup() %>%
  filter(count > 0) %>%
  type.convert(as.is = TRUE) %>% 
  uncount(count) %>%
  mutate(random_decimal = runif(n = nrow(.), min = 0, max = 1),
         random_normal = 1 + rnorm(n = nrow(.), sd = .1)) %>%
  group_by(cluster_id) %>%
  mutate(rank = row_number(desc(random_decimal))) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  mutate(median_household_income_noise = median_household_income * random_normal,
         residual =  median_household_income - median_household_income_noise) %>%
  group_by(race, cluster_id) %>%
  mutate(sum_residual = sum(residual),
         count_residual = sum(n()),
         adj_residual = sum_residual/ count_residual) %>%
  ungroup() %>%
  mutate(median_household_income_noise = median_household_income_noise + adj_residual) %>% 
  select(cluster_id_race, cluster_id, race, median_household_income_noise)


sample_qc_1b <- cluster_distribution %>%
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

semistratified_distribution <- bg_data_all %>%
  st_drop_geometry() %>%
  group_by(cluster_id_race, cluster_id, median_household_income) %>%
  summarize_at(vars(race_population_white, race_population_black, race_population_latino, race_population_asian_pacific_islander, race_population_multiracial_other, race_population_native_american),
               list(sum)) %>%
  ungroup() %>%
  pivot_longer(
    cols =c("race_population_white", "race_population_black", "race_population_latino","race_population_asian_pacific_islander", "race_population_multiracial_other", "race_population_native_american"),
    names_to = c("race")) %>%
  mutate(race = case_when(race == "race_population_latino" ~ 'Latino',
                          race == "race_population_white" ~ 'White',
                          race == "race_population_black" ~ 'Black',
                          race == "race_population_native_american" ~ 'Native American',
                          race == "race_population_asian_pacific_islander" ~ 'Asian Pacific Islander',
                          race == "race_population_multiracial_other" ~ 'Multiracial other')) %>%
  mutate(median_household_income_wt = median_household_income * value) %>%
  group_by(cluster_id_race, cluster_id, race) %>%
  summarize_at(vars(median_household_income_wt, value),
               list(sum)) %>%
  ungroup() %>%
  mutate(median_household_income = median_household_income_wt/value) %>%
  select(-one_of(c('median_household_income_wt'))) %>%
  group_by(cluster_id) %>%
  mutate(share = value/sum(value),
         count = round(value/sum(value) * 20, 0)) %>%
  ungroup() %>%
  filter(count > 0) %>%
  type.convert(as.is = TRUE) %>% 
  uncount(count) %>%
  mutate(random_decimal = runif(n = nrow(.), min = 0, max = 1),
         random_normal = 1 + rnorm(n = nrow(.), sd = .1)) %>%
  group_by(cluster_id) %>%
  mutate(rank = row_number(desc(random_decimal))) %>%
  ungroup() %>%
  filter(rank <= 10) %>%
  mutate(median_household_income_noise = median_household_income * random_normal,
         residual =  median_household_income - median_household_income_noise) %>%
  group_by(race, cluster_id) %>%
  mutate(sum_residual = sum(residual),
         count_residual = sum(n()),
         adj_residual = sum_residual/ count_residual) %>%
  ungroup() %>%
  mutate(median_household_income_noise = median_household_income_noise + adj_residual) %>% 
  select(cluster_id_race, cluster_id, race, median_household_income_noise)
?uncount
sample_qc_2b <- semistratified_distribution %>%
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



ggplot(data = control_qc, aes(x = household_income_bucket, y = city_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") + 
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') +
ggplot(data = sample_qc_1a, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') +
ggplot(data = sample_qc_1b, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom')

ggplot(data = control_qc, aes(x = household_income_bucket, y = city_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") + 
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') + 
ggplot(data = sample_qc_2a, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') +
ggplot(data = sample_qc_2b, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  labs(x = '', y = '') + coord_flip() + theme_classic()  + guides(fill=guide_legend(nrow=3,byrow=TRUE)) + theme(legend.position = 'bottom') 



ggplot(data = sample, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  theme(legend.position = 'bottom') +
ggplot(data = sample_qc2, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  theme(legend.position = 'bottom') +
ggplot(data = sample_qc3, aes(x = household_income_bucket, y = samp_shr, group = race)) +
  geom_bar(aes(fill = race, color = race), alpha = .9, stat="identity") +
  theme(legend.position = 'bottom') 

# QC Check ----------------------------------------------------------------

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

ggplot() +
  geom_sf(data = clusters, aes(fill = cluster_plurality_race), alpha = .2)+ 
  geom_sf(data = bg_data_100, aes(fill = cluster_id_race), alpha = 1, color = 'white') + 
  geom_sf(data = sample_points, color = 'black') +
  theme_void()
  
# Appendix ----------------------------------------------------------------

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
  
