# data
library(dplyr)
library(tidyverse)

# Global parameters
num_students <- 20
hist_bin_width <- 1000

# Sampling functions
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

# n students' samples concatenated into one dataframe and id'ed by student
simple_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
  simple_rs(people) %>% mutate(student_id = x)
})

stratified_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
  stratified_rs(people) %>% mutate(student_id = x)
})

cluster_samples <- map_dfr(.x = seq(1, num_students), .f = function(x) {
  cluster_rs(people) %>% mutate(student_id = x)
})

# Plots of sampled points (aggregated by student)
simple_hist <- ggplot() + 
  geom_histogram(data = simple_samples %>% 
                   group_by(student_id) %>% 
                   summarize(median_income = median(median_household_income_noise)) %>%
                   select(median_income),
                 color = 'black',
                 fill = 'white',
                 aes(x = median_income),
                 binwidth = hist_bin_width) + 
  theme_void()

stratified_hist <- ggplot() + 
  geom_histogram(data = stratified_samples %>% 
                   group_by(student_id) %>% 
                   summarize(median_income = median(median_household_income_noise)) %>%
                   select(median_income), 
                 aes(x = median_income),
                 binwidth = hist_bin_width)

cluster_hist <- ggplot() + 
  geom_histogram(data = cluster_samples %>% 
                   group_by(student_id) %>% 
                   summarize(median_income = median(median_household_income_noise)) %>%
                   select(median_income), 
                 aes(x = median_income),
                 binwidth = hist_bin_width)

simple_hist / stratified_hist / cluster_hist

# Plots of sampled points (not aggregated by student)
ggplot() + 
  geom_histogram(data = simple_samples,
                 aes(x = median_household_income_noise),
                 binwidth = hist_bin_width)

ggplot() + 
  geom_histogram(data = stratified_samples,
                 aes(x = median_household_income_noise),
                 binwidth = hist_bin_width)

ggplot() + 
  geom_histogram(data = cluster_samples,
                 aes(x = median_household_income_noise),
                 binwidth = hist_bin_width)

# Median of overall data
people %>% summarize(median(median_household_income_noise))

# Median of students' samples by method
simple_samples %>% 
  group_by(student_id) %>% 
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(median(median_income))

stratified_samples %>% 
  group_by(student_id) %>% 
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(median(median_income))

cluster_samples %>% 
  group_by(student_id) %>% 
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(median(median_income))

# Check function
check <- function() {
  
}


