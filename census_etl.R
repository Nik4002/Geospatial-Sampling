library(tidyverse)
library(sf)
library(tidycensus)

readRenviron("~/.Renviron")

vars <- c(# "B01003_001",
          # "B03002_002",
          "B03002_003",
          "B03002_004",
          "B03002_005",
          "B03002_006",
          "B03002_007",
          "B03002_008",
          "B03002_009",
          "B03002_012")

# Race / ethncity 5 year ACS for 2020
bgroup_data_race <- get_acs(year = 2020, geography = "block group", 
                            survey = 'acs5', variables = vars,
                            summary_var = 'B03002_001', 
                            cache_table = TRUE, 
                            state = '17', county = '031', 
                            geometry = FALSE) %>% 
  rename_all(list(tolower)) %>%
  mutate(variable_label = case_when(# variable == 'B01003_001' ~ 'Total_Population',
                                    # variable == 'B03002_002' ~ 'Total_Non_Hispanic',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Native',
                                    variable == 'B03002_006' ~ 'Asian',
                                    variable == 'B03002_007' ~ 'Pacific',
                                    variable == 'B03002_008' ~ 'Other',
                                    variable == 'B03002_009' ~ 'Two_or_More',
                                    variable == 'B03002_012' ~ 'Hispanic')) %>%
  group_by(geoid, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum)) %>%
  ungroup() %>% mutate(share = estimate/summary_est) %>%
  pivot_wider(id_cols = c(geoid,summary_est),
              names_from = c(variable_label), 
              values_from = c(share, estimate)) %>%
  rename_all(list(tolower)) %>%
  rename(total_population = summary_est) %>%
  filter(total_population != 0) %>%
  select(geoid, total_population, 
         share_asian, share_black, share_hispanic, share_native, share_white, share_other, share_pacific, share_two_or_more,
         estimate_asian, estimate_black, estimate_hispanic, estimate_native, estimate_white, estimate_other, estimate_pacific, estimate_two_or_more)

# Median Household Income 5 year ACS for 2020
bgroup_data_income <- get_acs(year = 2020, geography = "block group", 
                              survey = 'acs5', variables = c('B19013_001'),
                              summary_var = 'B25001_001',
                              cache_table = TRUE,
                              state = '17', county = '031', 
                              geometry = FALSE) %>%
  mutate(variable_label = case_when(variable == 'B19013_001' ~ 'Median household income in the past 12 months')) %>%
  mutate(weight = estimate*summary_est)%>%
  rename_all(list(tolower)) %>%
  select(geoid, estimate, weight, summary_est) %>%
  rename(household_income = estimate, household_income_num = weight, household_income_denom = summary_est)