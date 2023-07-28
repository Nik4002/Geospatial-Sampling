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
    simple_rs(people) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
    select(median)
  
  stratified_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    stratified_rs(people) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
    select(median)
  
  cluster_medians <- map_dfr(.x = seq(1, num_students), .f = function(x) {
    cluster_rs(people) %>% mutate(student_id = x)
  }) %>% 
    group_by(student_id) %>% 
    summarize(median = median(median_household_income_noise)) %>%
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

check(people)


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
  geom_vline(xintercept = people %>% summarize(median(median_household_income_noise)))

stratified_hist <- ggplot() + 
  geom_histogram(data = stratified_samples %>% 
                   group_by(student_id) %>% 
                   summarize(median_income = median(median_household_income_noise)) %>%
                   select(median_income), 
                 color = 'black',
                 fill = 'white',
                 aes(x = median_income),
                 binwidth = hist_bin_width)

cluster_hist <- ggplot() + 
  geom_histogram(data = cluster_samples %>% 
                   group_by(student_id) %>% 
                   summarize(median_income = median(median_household_income_noise)) %>%
                   select(median_income), 
                 color = 'black',
                 fill = 'white',
                 aes(x = median_income),
                 binwidth = hist_bin_width)

simple_hist
simple_hist / stratified_hist / cluster_hist

# Median of overall data
people %>% summarize(median(median_household_income_noise))

# Mean of students' samples by method
simple_samples %>%
  group_by(student_id) %>%
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(mean(median_income))

stratified_samples %>%
  group_by(student_id) %>%
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(mean(median_income))

cluster_samples %>%
  group_by(student_id) %>%
  summarize(median_income = median(median_household_income_noise)) %>%
  select(median_income) %>%
  summarize(mean(median_income))







(simple_medians_bar <- ggplot(data = simple_samples %>% 
                         group_by(student_id) %>% 
                         summarize(median_income = median(median_household_income_noise)) %>%
                         select(median_income), aes(x = median_income, # y = values, 
                                                     #fill = class_urban_hierarchy)) +
                                                     #fill = class_urban_paper
                                                    )) +
    geom_bar(stat='identity') + 
    scale_x_discrete(expand = c(0.05,0.05)) +
    # scale_y_continuous(expand = c(0, 0), labels = label_comma(accuracy = 1L, scale =  0.000001, suffix = "M") ) +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    labs(x = expression("Median Income, $") ,
         y = 'Number of students') +
    theme_classic() + 
    theme(legend.position = 'bottom',
          legend.title = element_blank()))

(medians_density <- ggplot(data = histogram_buildings, 
                         aes(x = bin_label, y = share, 
                             group = class_urban_paper)) +
    geom_smooth(linewidth = 1.5, 
                #aes(fill = class_urban_hierarchy, color = class_urban_hierarchy), se = FALSE,
                aes(fill = class_urban_paper, color = class_urban_paper), se = FALSE,
                method = 'loess', span = 0.4, alpha = .9)+
    scale_x_discrete(expand = c(0.05,0.05)) +
    scale_fill_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    scale_color_manual(values = c('#08519c','#9ecae1','#006d2c','#a1d99b')) +
    labs(x = expression("Building footprint area, m"^2) ,
         #expression(Building ~ footprint ~ area ~ m^2),
         #parse(text='Building~~footprint~~area~~(scriptstyle(m^{2}))'),
         y = 'Density') +
    theme_classic() + 
    theme(legend.position = 'none',
          legend.title = element_blank()) + guides(fill = "none", color = 'none') )

