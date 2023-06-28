library(tidyverse)
library(sf)
library(tidycensus)

readRenviron("~/.Renviron")

# Race / ethncity 5 year ACS for 2020
bgroup_data_race <- get_acs(year = 2020, geography = "block group", 
                            survey = 'acs5', variables = c('B03002_012', 'B03002_003', 'B03002_004', 'B03002_005', 'B03002_006', 'B03002_007', 'B03002_008', 'B03002_009'),
                            summary_var = 'B03002_001', 
                            cache_table = TRUE, 
                            state = '17', county = '031', 
                            geometry = FALSE) %>% 
  rename_all(list(tolower)) %>%
  mutate(variable_label = case_when(variable == 'B03002_012' ~ 'Latino',
                                    variable == 'B03002_003' ~ 'White',
                                    variable == 'B03002_004' ~ 'Black',
                                    variable == 'B03002_005' ~ 'Other',
                                    variable == 'B03002_006' ~ 'Asian',
                                    variable == 'B03002_007' ~ 'Other',
                                    variable == 'B03002_008' ~ 'Other',
                                    variable == 'B03002_009' ~ 'Other')) %>%
  group_by(geoid, variable_label, summary_est) %>% 
  summarize_at(vars(estimate), list(sum)) %>%
  ungroup() %>% mutate(share = estimate/summary_est) %>%
  pivot_wider(id_cols = c(geoid,summary_est),
              names_from = c(variable_label), 
              values_from = c(share, estimate)) %>%
  rename_all(list(tolower)) %>%
  rename(total_population = summary_est) %>%
  select(geoid, total_population, 
         share_asian, share_black, share_latino, share_other, share_white,
         estimate_asian, estimate_black, estimate_latino, estimate_other, estimate_white)