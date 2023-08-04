

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
library(gtExtras)
library(ggpmisc)
# utilities
library(janitor)
library(conflicted)

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
options(tigris_use_cache = TRUE)

set.seed(seed = 100)


# Directory ---------------------------------------------------------------

wd_input = '/Users/nm/Desktop/Projects/work/skew-the-script/inputs.nosync'
# wd_output = '/Users/nm/Desktop/Projects/work/skew-the-script/outputs.nosync'
wd_output = '/Users/nikhilpatel/Documents/Projects/Geospatial_Sampling/plots'

# write_excel_csv(x = agglos_df, file = paste0(wd_output,'/data/agglomeration_data.csv'))

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
  filter(city_rank <= 100)
rm(places_pop)

places_list <- places(cb = TRUE, year = 2020) %>%
  st_transform(4326) %>%
  inner_join(., places_pop_rank, by = c('GEOID'='placeid')) %>%
  rename_all(tolower) %>%
  select(geoid, name, popestimate2020, popestimate2021, popestimate2022, city_rank, geometry) %>% 
  separate(data = ., col = 'name', into = 'name_short', sep =  "(\\[|-|]|/|[(])", remove = TRUE, extra = "drop")

state_xwalk <- tidycensus::fips_codes %>%
  mutate(county_fips = paste0(state_code,county_code))

us_county <- get_acs(year = 2020, geography = "county", variables = "B01003_001", 
                     geometry = TRUE, shift_geo = FALSE) %>%
  rename_all(tolower) %>% 
  select(geoid, estimate) %>%
  rename(county_fips = geoid) %>%
  left_join(., state_xwalk, by = c('county_fips' = 'county_fips')) %>% 
  select(state, county_fips, state_code, county_code) %>%
  st_transform(4326) 

county_place_map <- places_list %>%
  st_join(., us_county, left = FALSE) %>%
  arrange(desc(popestimate2022)) %>%
  mutate(name_short_strip = gsub("\\s+|\\.|\\/", "_", tolower(name_short)))

finished_list <- gsub(".pdf", "",list.files(path = wd_output, pattern = "\\.pdf$", full.names = FALSE))

remaining_list <- county_place_map %>% 
  filter(!(name_short_strip %in% finished_list))

# Run loop over places ----------------------------------------------------

# for (i in unique((remaining_list %>% filter(city_rank >= 97))$geoid)) {
  # i <- 1714000 # Chicago
  # i <- 2255000 # New Orleans (wide city)
  # i <- 1571550 # Urban Honolulu
  # i <- 5548000 # Madison
  # i <- 2507000 # Boston
  # i <- "0820000" # Denver
  i <- "0675000" # Stockton
  
  place_name <- county_place_map %>% filter(geoid == i) %>% st_drop_geometry() %>%
    select(name_short) %>% pull() %>% unique()
  
  print(place_name)
  
  places_geo <- places_list %>% filter(geoid == i) %>%
    rename(placeid = geoid) %>% select(placeid, geometry)
  
  city_fips <- county_place_map %>% filter(geoid == i) %>% st_drop_geometry() %>%
    select(state_code, county_code) 
  
  state_fips_2 <- city_fips %>% select(state_code) %>% pull()
  county_fips_3 <- city_fips %>% select(county_code) %>% pull()
  
  # Download tract geometries
  
  tract_data_geo <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", 
            survey = 'acs5', variables = c('B19013_001'),
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
  
  # tract_data %>% select(placeid) %>% 
  #   st_intersection(., places_geo %>% select(geometry)) %>%
  #   plot()
  
  # Download data, recode variables to race / ethnicity categories, then aggregate
  
  tract_data_race <- map2_dfr(.x = state_fips_2, .y = county_fips_3, .f = function(x , y) {
    get_acs(year = 2020, geography = "tract", 
            survey = 'acs5', variables = c('B03002_012', 'B03002_003', 'B03002_004', 'B03002_005', 'B03002_006', 'B03002_007', 'B03002_008', 'B03002_009'),
            summary_var = 'B03002_001', 
            cache_table = TRUE, 
            state = x, county = y,
            #state = '17', county = '031',  # PARAMETERIZE
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
  
  # Join data and geometries
  # tract_data <- tract_data_geo %>%
  #   left_join(., tract_data_plurality_race, by = c('geoid' = 'geoid')) %>%
  #   filter(!is.na(plurality_race))  %>%
  #   st_transform(4326) %>%
  #   st_join(., places_geo %>% st_transform(4326), left = FALSE)
  
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
  
  # # Create dataframe of plurality race for each block group
  # tract_data_race_rank <- tract_data_race %>%
  #   filter(plurality_rank == 1) %>%
  #   rename(plurality_race_population = race_population, 
  #          plurality_race_share = race_share,
  #          plurality_race = race) %>%
  #   select(geoid, plurality_race, plurality_race_population, plurality_race_share)
  
  # Create wide dataframe of race shares for each block group
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
  
  # Join together block group data
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
  
  #tract_data_clusters %>%
  
  # fix so that cluster count > number of tracts
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
    # mutate(share = value / sum(value),
    #        below_10 = case_when(share < .1 ~ .1, TRUE ~ 0),
    #        realloc_factor = 1-sum(below_10)) %>%
    # group_by(below_10) %>%
    # mutate(share_realloc = (share/sum(share))*realloc_factor) %>%
    # ungroup() %>%
    # mutate(share_realloc = case_when(share < .1 ~ .1, TRUE ~ share_realloc),
    #        share_realloc = round(share_realloc, 1),
    #        cluster_count = 10*share_realloc,
    #        cluster_count = 10*(cluster_count/sum(cluster_count))) %>% ## NORMALIZE COUNT TO 10
    uncount(cluster_count) %>%
    sample_n(size = 10) %>% # come up with way to force minority clusters to always be sampled
    group_by(race) %>%
    tally(name = 'cluster_count') %>%
    ungroup() 
    
  sample_points <- purrr::map_dfr(
    .x = city_distribution %>% select(race) %>% pull(), 
    .f = function(i) {

      print(i)
      
      (num_points <- city_distribution %>%
        filter(race == i) %>% 
        select(cluster_count) %>% pull() )
      
      if (num_points > 1) { 
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
                                      #filter(total_population > 0) %>%
                                      #mutate(lon = scale(lon, center = TRUE, scale = TRUE)*total_population,
                                      #       lat = scale(lat, center = TRUE, scale = TRUE)*total_population) %>%
                                      mutate(lon = scale(lon, center = TRUE, scale = TRUE),
                                             lat = scale(lat, center = TRUE, scale = TRUE)) %>%
                                      select(lon, lat) %>%
                                      as.matrix(),
                                    centers = num_points)
      } else {
        k_clusters <- tract_data_clusters %>%
          filter(cluster_plurality_race == i) %>%
          mutate(cluster = 1) %>% select(cluster)
      }
      
      # XY-only k-means
      # k_clusters <- stats::kmeans(x = bg_data_clusters %>%
      #                               filter(cluster_plurality_race == i) %>%
      #                               st_centroid() %>% st_coordinates(),
      #                             centers = num_points)
      
      race_tract_groups <- tract_data_clusters %>% 
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
    geom_sf(data = tract_data_clusters, aes(fill = cluster_plurality_race)) +
    geom_sf(data = sample_points, color = 'black')
  
  tract_data_sample <- sample_points %>% 
    st_transform(3395) %>%
    st_join(., tract_data_all %>% st_transform(3395), left = TRUE,
            join = st_nn, k = 10) %>% #, returnDist = TRUE
    st_transform(4326)# %>%
  #st_drop_geometry() %>%
  # group_by(cluster_id) %>%
  # sample_frac(size = .5, weight = total_population) %>%
  # ungroup() 
  
  tract_data_all_geo <- tract_data_all %>% st_transform(3395) %>%
    st_join(., sample_points %>% st_transform(3395), left = TRUE,
            join = st_nn, k = 1) %>% 
    st_transform(4326)

  # Regional sample option
  ggplot() +
    geom_sf(data = tract_data_all_geo, aes(fill = cluster_id), color = 'white') +
    geom_sf(data = sample_points, color = 'black') +
    theme_void()
  
  # -------------------------------------------------------------------------
  
  # Clusters
  clusters <- tract_data_clusters %>%
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
  # 
  # # Clusters by race
  # ggplot() +
  #   geom_sf(data = clusters, aes(fill = cluster_plurality_race), color = 'white')+ 
  #   theme_void()
  
  # Roads and water ---------------------------------------------------------
  
  buffer <- 0.01
  bbox <- clusters %>% st_bbox()
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
  rm(polygon_points, polygon)
    
  roads_layer <- tigris::primary_roads(year = 2020) %>%
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
  
  set.seed(seed = 100)
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
      print('Race / ethnicity absent from synthetic count data') ## if this happens make it retry -- happens when there is a group with less than 1pct
    }
  } else { 
    print('No missing race / ethnicities.') 
  }
  
  # Viz ---------------------------------------------------------------------
  
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
  
  clusters_padding <- clusters_10 %>%
    st_difference(., clusters_10 %>% 
                    st_boundary() %>% st_as_sf() %>%
                    st_transform(3395) %>%
                    st_buffer(., dist = 400) %>%
                    st_transform(4326) %>% st_make_valid() %>% st_union() %>% st_make_valid() ) 
  
  water_remove <- water_layer %>% st_transform(3395) %>%
    st_buffer(., dist = 10) %>%
    st_simplify(., preserveTopology = TRUE, dTolerance = units::set_units(10,m)) %>% 
    st_union(.) %>% st_as_sf() %>%
    st_transform(4326) %>% st_make_valid()
  
  if (nrow(water_remove) > 0 ) {
    clusters_padding <-   clusters_padding %>%
      st_difference(., water_layer %>% st_transform(3395) %>%
                      st_buffer(., dist = 10) %>%
                      st_simplify(., preserveTopology = TRUE, dTolerance = units::set_units(10,m)) %>% 
                      st_union(.) %>%
                      st_transform(4326) %>% st_make_valid())
  }
  
  set.seed(seed = 100)
  dots <- sf::st_sample(clusters_padding %>% st_transform(3395), size = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100), by_polygon = TRUE, type = 'regular') %>%
    st_as_sf() %>%
    st_transform(4326) %>%
    st_join(., clusters_10, left = FALSE,  join = st_within) # , largest = TRUE
  
  ggplot() +
    geom_sf(data = clusters_10, color = 'black', fill = 'white', alpha = 0, linewidth = .2) +
    geom_sf(data = dots, color = 'black', fill = 'black', alpha = 1, size = 1, linewidth = .2) +
    theme_void()
  
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
  
  # inner_box <- clusters_10 %>% st_bbox()
  # outer_box <- clusters_10 %>% st_transform(3395) %>% st_buffer(500) %>% st_transform(4326) %>% st_bbox()
  # ggrepel::geom_text_repel(data = clusters_10 ,
  #                 seed = 1,# box.padding = 10, max.iter = 1000, 
  #                 segment.square  = FALSE, segment.inflect = FALSE,  max.overlaps = Inf, 
  #                 force = 10, direction = 'x',
  #                 aes(label = region_loc, geometry = geometry), stat = "sf_coordinates",
  #                 size = 3, vjust =.5, color = '#333333', fontface='bold') + 
  
  
  
  color_vec <- c('#F5870C', '#4472C4', '#06c049', '#70309F', '#fece0a', '#FF0000')
  
  (map <- ggplot() +
      geom_sf(data = bbox, fill = 'white', alpha = 1) +
      geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
      geom_sf(data = clusters_10, alpha = 0, linewidth = .6) +
      geom_sf(data = roads_layer, color = '#d2d2d2', alpha = 1, linewidth = .6) +
      geom_sf(data = bbox, color = '#999999', alpha = 0, linewidth = 1) +
      geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
      # geom_sf_text(data = clusters_10, aes(label = paste0('Region ',region_loc)), color =  '#333333',
      #              alpha = .8, size = 4, fontface='bold') +
      geom_sf(data = synthetic_sample_points %>%
                mutate(race = case_when(race == 'Asian/Pacific Islander' ~ 'Asian',
                                        race == 'Multiracial/Other' ~ 'Multiracial',
                                        race == 'Native American' ~ 'Native', 
                                        TRUE ~ as.character(race))) %>%
                mutate(race = factor(race, levels = c("Asian", "Black", "Latino/a", "Multiracial", "Native", "White"))), 
              aes(color = race, fill = race, size = median_household_income_noise ), 
              alpha = .8, linewidth = .2) + #size = 6.5,
      ggrepel::geom_text_repel(data = synthetic_sample_points,
                               seed = 1, segment.curvature = 0, point.padding = 0, box.padding = 0, max.iter = 1000, segment.square  = FALSE, segment.inflect = FALSE, 
                               min.segment.length = 0, max.overlaps = Inf, force = .01, force_pull = 2, aes(x = lon, y = lat, label = id), 
                               size = 3, vjust =.5, color = 'white', fontface='bold') + 
      guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
      scale_fill_manual(values = color_vec, name = 'Race/\nethnicity') +
      scale_color_manual(values = color_vec, name = 'Race/\nethnicity' ) +
      scale_size_binned(name = 'Household\nincome', range = c(2.5, 12), 
                        n.breaks = 4,
                        labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
      guides(color = guide_legend(override.aes = list(size = 6, alpha = 1))) +
      labs(subtitle = place_name, caption = "*Asian represents Asian and Pacific Islanders, Native represents Native Americans,\nand Multiracial represents people of 2 or more races or who belong to another race.*") +
      theme_void() + theme(legend.position = 'right',
                           legend.justification = "top",
                           plot.subtitle = element_text(size = 15, face = 'bold', hjust = .5),
                           plot.caption = element_text(size = 12, hjust = 0.5, vjust = 0.5),
                           legend.title = element_text(size = 12, face = 'bold'),
                           legend.text = element_text(size = 12),
                           legend.margin=margin(t=20,r=0,b=0,l=5),
                           legend.box.margin=margin(0,0,0,0),
                           plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
                           legend.box = 'vertical'
      ))
  
  # map
  
  design1 <- "
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
    AAAAAAAAAAAAB
  "
  
  map <- map + guide_area() + plot_layout(design = design1)
  
  map
  
  #     theme_void() + theme(legend.position = 'right',
  #                          legend.justification = "top",
  #                          plot.subtitle = element_text(size = 15, face = 'bold', hjust = .5),
  #                          plot.caption = element_text(size = 12, hjust = 0),
  #                          legend.title = element_text(size = 12, face = 'bold'),
  #                          legend.text = element_text(size = 12),
  #                          panel.grid = element_blank(),
  #                          panel.border = element_blank(),
  #                          panel.background = element_blank(),
  #                          legend.margin=margin(t=20,r=17,b=0,l=0),
  #                          legend.box.margin=margin(0,0,0,0),
  #                          plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
  #                          legend.box = 'vertical'
  #     ))
  # 
  # # map
  # 
  # design1 <- "
  #   AAAAAAAB
  #   AAAAAAAB
  #   AAAAAAAB
  # "
  # 
  # map <- map + guide_area() + plot_layout(design = design1)
  # 
  # map
  
  # -------------------------------------------------------------------------
  
    
  #   
  #   (viz <- map + guide_area() + table_1_5 + table_6_10 + 
  #       plot_layout(design = design, guides = "collect") )
  #   
  #   (pathfile = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ),'.pdf'))
  #   
  #   # ggsave(plot = viz, filename = pathfile, width = 8.5, height = 11) # dpi = 300,
  #   viz
  
  # map

  # edges <- st_bbox(clusters_10)
  # if (edges$xmax - edges$xmin > edges$ymax - edges$ymin) { # If width is greater than height
  #   ggsave(plot = map + plot_layout(guides = 'collect'), 
  #          filename = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ), '_map', '.pdf'), 
  #          width = 11, height = 8.5) # dpi = 300,
  # } else {
  #   ggsave(plot = map + plot_layout(guides = 'collect'), 
  #          filename = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ), '_map', '.pdf'), 
  #          width = 8.5, height = 11) # dpi = 300,
  # }
  
  # -------------------------------------------------------------------------
  
  sample_table <- synthetic_sample_points %>% st_drop_geometry() %>%
    select(region_loc, id, race, median_household_income_noise) %>% arrange(region_loc, id) %>%
    mutate(median_household_income_noise = paste0('$',comma(median_household_income_noise, accuracy = 1L)) 
    ) %>%
    mutate(race = case_when(race == 'Asian/Pacific Islander' ~ 'Asian',
                            race == 'Multiracial/Other' ~ 'Other',
                            race == 'Native American' ~ 'Native',
                            TRUE ~ as.character(race)
    ) ) %>%
    rename(`Region` = region_loc,
           `ID` = id,
           `Race/\nethnicity` = race, 
           `Household\nincome` = median_household_income_noise) 
  
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
  
  (region_map <- ggplot() +
     geom_sf(data = water_layer, color = '#d1edff', fill = '#d1edff', alpha = 1, linewidth = .6) +
     geom_sf(data = clusters_10, fill = '#cfd0c5', alpha = 0, linewidth = .6) +
     # geom_sf(data = bbox, fill = '#cfd0c5', alpha = 0.5) + 
     geom_sf(data = roads_layer, color = 'white', alpha = 1, linewidth = .4) +
     geom_sf(data = bbox, alpha = 0, linewidth = 1) + 
     geom_sf(data = clusters_10, color = '#333333', alpha = 0, linewidth = .4) +
     geom_text(data = clusters_10 %>% st_difference(., clusters_10 %>% st_boundary() %>% st_buffer(500) %>% st_simplify()) %>% filter(c(cluster_id == cluster_id.1)) %>% 
                 mutate(lon = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
                        lat = map_dbl(geometry, ~st_point_on_surface(.x)[[2]])),
               aes(x = lon, y = lat, label = region_loc), 
               fontface = 'bold',
               size = 6) + 
     # geom_sf_text(data = clusters_10, aes(label = paste0('Region ',region_loc)), color =  '#333333',
     #              alpha = .8, size = 4, fontface='bold') +
     # geom_sf(data = synthetic_sample_points, 
     #         aes(color = race, fill = race, size = median_household_income_noise ), 
     #         alpha = .8, linewidth = .2) + #size = 6.5,
     # ggrepel::geom_text_repel(data = synthetic_sample_points,
     #                          seed = 1, segment.curvature = 0, point.padding = 0, box.padding = 0, max.iter = 1000, segment.square  = FALSE, segment.inflect = FALSE, 
     #                          min.segment.length = 0, max.overlaps = Inf, force = .01, force_pull = 2, aes(x = lon, y = lat, label = id), 
     #                          size = 3, vjust =.5, color = 'white', fontface='bold') + 
     # guides(color = guide_legend(override.aes = list(size = 6, alpha =1))) +
     # scale_fill_manual(values = color_vec, name = 'Race / ethnicity') +
     # scale_color_manual(values = color_vec, name = 'Race / ethnicity' ) +
     # scale_size_binned(name = 'Household income', range = c(2.5, 12), 
     #                   n.breaks = 4,
     #                   labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
     # guides(color = guide_legend(override.aes = list(size = 6, alpha = 1))) +
     labs(subtitle = "Regions") + 
     theme_void() + theme(panel.grid = element_blank(),
                          plot.subtitle = element_text(size = 15, face = 'bold', hjust = .5),
                          panel.border = element_blank(),
                          panel.background = element_blank(),
                          plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
     )
  )
  
  design2 <- "
    AAAAAAA
    AAAAAAA
    AAAAAAA
    CCCCCCC
    DDDDDDD
  "
  
  # region_map
  
  regions_and_table <- region_map + table_1_5 + table_6_10 + 
    plot_layout(design = design2, guides = "collect")

  # ggsave(plot = regions_and_table, 
  #        filename = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ), '_table', '.pdf'), 
  #        width = 8.5, height = 11) # dpi = 300,

  # Teacher's Key -----------------------------------------------------------
  
  # n students' samples concatenated into one dataframe and id'ed by student
  judgment_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    judgment(synthetic_sample_points) %>% mutate(student_id = x)
  }) %>% st_drop_geometry()
  
  simple_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    simple_rs(synthetic_sample_points) %>% mutate(student_id = x)
  }) %>% st_drop_geometry()
  
  stratified_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    stratified_rs(synthetic_sample_points) %>% mutate(student_id = x)
  }) %>% st_drop_geometry()
  
  cluster_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    cluster_rs(synthetic_sample_points) %>% mutate(student_id = x)
  }) %>% st_drop_geometry()
  
  # Plots of sampled points (aggregated by student)
  
  bin_width <- 5000
  lo <- floor(min(synthetic_sample_points$median_household_income_noise)/bin_width) * bin_width
  hi <- ceiling(max(synthetic_sample_points$median_household_income_noise)/bin_width) * bin_width

  judgment_hist <- ggplot(judgment_samples %>%
                            group_by(student_id) %>%
                            summarize(median_income = median(median_household_income_noise)) %>%
                            # mutate(income_bin = floor(median_income/bin_width) * bin_width/1000) %>%
                            mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                          median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                          median_income >= hi ~ hi + bin_width)) %>%
                            group_by(income_bin) %>%
                            mutate(student_order = row_number()), 
                          aes(y=student_order, x=income_bin)) + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
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
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 10) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Judgment Sample") +
    ggplot2::annotate("text",
                      x = max(synthetic_sample_points$median_household_income_noise), y = 7,
                      label = "Median income of 100 households",
                      color = "#4472C4", fontface = "bold", hjust = 1) +
    ggplot2::annotate("text",
                      x = max(synthetic_sample_points$median_household_income_noise), y = 8,
                      label = "Mean of medians of students' samples",
                      color = "#F5870C", fontface = "bold", hjust = 1)

  simple_hist <- ggplot(simple_samples %>%
                              group_by(student_id) %>%
                              summarize(median_income = median(median_household_income_noise)) %>%
                              # mutate(income_bin = floor(median_income/bin_width) * bin_width/1000) %>%
                              mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                            median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                            median_income >= hi ~ hi + bin_width)) %>%
                              group_by(income_bin) %>%
                              mutate(student_order = row_number()), 
                            aes(y=student_order, x=income_bin)) + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
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
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 10) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Simple Random Sample (SRS)")

  
  stratified_hist <- ggplot(stratified_samples %>%
           group_by(student_id) %>%
           summarize(median_income = median(median_household_income_noise)) %>%
           # mutate(income_bin = floor(median_income/bin_width) * bin_width/1000) %>%
           mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                         median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                         median_income >= hi ~ hi + bin_width)) %>%
           group_by(income_bin) %>%
           mutate(student_order = row_number()), 
         aes(y=student_order, x=income_bin)) + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
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
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 10) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Stratified Random Sample")
  
  cluster_hist <- ggplot(cluster_samples %>%
                              group_by(student_id) %>%
                              summarize(median_income = median(median_household_income_noise)) %>%
                              # mutate(income_bin = floor(median_income/bin_width) * bin_width/1000) %>%
                              mutate(income_bin = case_when(median_income < lo ~ lo - bin_width,
                                                            median_income >= lo & median_income < hi ~ floor(median_income/bin_width) * bin_width + 2500,
                                                            median_income >= hi ~ hi + bin_width)) %>%
                              group_by(income_bin) %>%
                              mutate(student_order = row_number()), 
                            aes(y=student_order, x=income_bin)) + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
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
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    geom_text(aes(y = student_order - 0.5, x= income_bin), label = '×', color = 'black', size = 10) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Cluster Random Sample")
  
  (key <- judgment_hist / simple_hist / stratified_hist / cluster_hist)
  
  # ggsave(plot = key,
         # filename = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ), '_key', '.pdf'),
         # width = 8.5, height = 11) # dpi = 300,
  
  # judgment_hist / simple_hist / stratified_hist / cluster_hist
  
  # Blank keys  -------------------------------------------------------------
  
  blank_judgment_hist <- ggplot() + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Judgment Sample")
  
  blank_simple_hist <- ggplot() + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Simple Random Sample (SRS)")
  
  blank_stratified_hist <- ggplot() + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Stratified Random Sample")
  
  blank_cluster_hist <- ggplot() + 
    # geom_density(data = synthetic_sample_points, 
    #              aes(x = median_household_income_noise, y = after_stat(density) * (max(synthetic_sample_points$median_household_income_noise) - min(synthetic_sample_points$median_household_income_noise)))) +
    scale_x_continuous(breaks = seq(lo, hi, by = bin_width),
                       limits = c(lo, hi),
                       labels = label_dollar(accuracy = 1L, scale =  0.001, suffix = "K")) +
    scale_y_continuous(breaks = seq(0, 10), limits = c(0, 8)) + 
    labs(x = "Household income", y = 'Student count') + 
    theme_classic() + 
    theme(axis.text.x=element_text(size=6),
          axis.text.y=element_text(size=6)) + 
    ggtitle("Cluster Random Sample")
  
  blank_key <- blank_judgment_hist / blank_simple_hist / blank_stratified_hist / blank_cluster_hist
  
  # ggsave(plot = blank_key, 
  #        filename = paste0(wd_output,'/',gsub("\\s+|\\.|\\/", "_", tolower(place_name) ), '_blank_key', '.pdf'), 
  #        width = 8.5, height = 11) # dpi = 300,
  
  # QC ----------------------------------------------------------------------
  if (!check(synthetic_sample_points)) {
    warning(sprintf("%s did not pass the QC check", place_name))
  } else {
    sprintf("%s passed the QC check!", place_name)
  }
  
# } # End of loop

# Sampling functions
judgment <- function(pop) {
  return(pop %>% slice_sample(n = 10, weight_by = median_household_income_noise ** 2))
}

simple_rs <- function(pop) {
  return(pop %>% slice_sample(n = 10))
}

stratified_rs <- function(pop) {
  return(pop %>% group_by(cluster_id) %>% slice_sample(n = 1) %>% ungroup())
}

cluster_rs <- function(pop) {
  cluster <- unique(pop$cluster_id) %>% sample(., 1)
  return(pop %>% filter(cluster_id == cluster))
}

biased <- function(medians, distance, true_median) {
  # A <- max(c(lo, hi))
  # B <- abs(true_median - mean(medians))
  # return(B/A > 0.25) # Change this to be 25% of the distance from the true median to the furthest median in cluster sampling
  return(distance/(abs(true_median - mean(medians))) > 0.25)
}

# Check function
check <- function(pop, n_students = 20) {
  # Generate samples
  # Aggregate samples
  simple_medians <- map_dfr(.x = seq(1, n_students), .f = function(x) {
    simple_rs(pop) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median)
  
  stratified_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    stratified_rs(pop) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median)
  
  cluster_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    cluster_rs(pop) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
    ungroup() %>%
    select(median)
  
  # Calculate variances and check rank
  simple_var <- var(simple_medians$median)
  stratified_var <- var(stratified_medians$median)
  cluster_var <- var(cluster_medians$median)
  # simple_var <- var(simple_medians)
  # stratified_var <- var(stratified_medians)
  # cluster_var <- var(cluster_medians)
  
  if (stratified_var > simple_var | simple_var > cluster_var) {
    return(FALSE)
  }
  
  # Check bias
  true_median <- median(pop$median_household_income_noise)
  all_medians <- bind_rows(simple_medians, stratified_medians, cluster_medians)
  lo <- min(all_medians$median)
  hi <- max(all_medians$median)
  distance <- max(abs(lo - true_median), abs(hi - true_median))
  return(abs(true_median - mean(simple_medians$median))/distance <= 0.25 &
           abs(true_median - mean(stratified_medians$median))/distance <= 0.25 &
           abs(true_median - mean(cluster_medians$median))/distance <= 0.25)
}

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
