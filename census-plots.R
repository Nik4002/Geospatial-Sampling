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

install.packages('lwgeom')
library(lwgeom)

# -------------------------------------------------------------------------

# census_api_key("YOUR KEY GOES HERE", install = TRUE)
readRenviron("~/.Renviron") # create an .Renviron > cd > touch .Renviron 
# Create test data.
data <- data.frame(
  category=c("A", "B", "C"),
  count=c(10, 60, 30)
)

# Compute percentages
data$fraction = data$count / sum(data$count)

# Compute the cumulative percentages (top of each rectangle)
data$ymax = cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin = c(0, head(data$ymax, n=-1))

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=category)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) # Try to remove that to see how to make a pie chart

# Join cluster assignments from bg_data_all to bg_data_race
bg_plot_data_race <- inner_join(x = bg_data_race,
          y = bg_data_all %>% select(c('geoid', 'cluster_id')) %>% st_drop_geometry(), 
          by = c('geoid' = 'geoid'))

# Group by cluster and race, calculate totals by race per cluster, and ungroup
#bg_plot_data_race <- bg_plot_data_race %>%
#  group_by(race, cluster_id) %>%
#  mutate(race_pop_in_cluster = sum(race_population, na.rm = TRUE)) %>%
#  ungroup()

# Group by cluster and calculate total and shares by cluster, and ungroup?
#bg_plot_data_race <- bg_plot_data_race %>%
#  group_by(cluster_id) %>%
#  mutate(cluster_pop = sum(race_population, na.rm = TRUE)) %>%
#  # mutate(race_share_in_cluster = race_pop_in_cluster/cluster_pop) %>%
#  ungroup()

# # Calculate n-tiles for donut chart locations
# region_positions <- bg_data_all %>%
#   group_by(cluster_id) %>%
#   dplyr::summarize(geometry = st_union(geometry)) %>%
#   ungroup() %>%
#   mutate(centroid = sf::st_centroid(geometry)) %>%
#   mutate(lon = map_dbl(centroid, ~st_point_on_surface(.x)[[1]]) ,
#          lat = map_dbl(centroid, ~st_point_on_surface(.x)[[2]]) ) %>%
#   mutate(vertical = ntile(desc(lat), 5)) %>%
#   group_by(vertical) %>%
#   mutate(horizontal = ntile(lon, 2)) %>%
#   ungroup() %>%
#   mutate(plot_loc = vertical + 5 * (horizontal - 1))
#   #ungroup()

# Generate a data frame with clusters and where their plots are located
clusters <- bg_data_all %>%
  group_by(cluster_id) %>%
  dplyr::summarize(geometry = st_union(geometry)) %>%
  ungroup() %>%
  mutate(centroid = sf::st_centroid(geometry)) %>%
  mutate(lon = map_dbl(centroid, ~st_point_on_surface(.x)[[1]]) ,
         lat = map_dbl(centroid, ~st_point_on_surface(.x)[[2]]) ) %>%
  mutate(vertical = ntile(desc(lat), 5)) %>%
  group_by(vertical) %>%
  mutate(horizontal = ntile(lon, 2)) %>%
  ungroup() %>%
  mutate(plot_loc = vertical + 5 * (horizontal - 1)) %>%
  select(cluster_id, geometry, plot_loc)

cluster_plot_data_race <- bg_plot_data_race %>% 
  group_by(race, cluster_id) %>% 
  summarize_at(vars(race_population), list(sum), na.rm = TRUE) %>%
  ungroup() %>%
  group_by(cluster_id) %>%
  mutate(share = race_population / sum(race_population)) %>%
  ungroup() %>%
  left_join(., clusters %>% select(cluster_id, plot_loc) %>% st_drop_geometry(), by = c('cluster_id'='cluster_id'), relationship = "many-to-many")
# mutate(race_share_in_cluster = race_pop_in_cluster/cluster_pop)
# filter(cluster_id == "latino_1") %>% mutate(ymax = cumsum(race_share_in_cluster)) %>% mutate(ymin = c(0, head(ymax, n=-1)))

cluster_plot_data_race <- cluster_plot_data_race %>%
  group_by(cluster_id) %>%
  # arrange(desc(share)) %>%
  mutate(race = factor(race, levels = c('White', 'Latino', 'Black', 'Asian Pacific Islander', 'Multiracial other', 'Native American'))) %>%
  arrange(plot_loc, race) %>%
  mutate(ymax = cumsum(share)) %>%
  mutate(ymin = c(0, head(ymax, n=-1))) %>%
  ungroup()

color_vec <- c('#4472C4',
  '#70AC47',
  '#FF0000',
  '#FEC000',
  '#70309F',
  '#F5870C')

# Plot charts 1-5
left_plots <- ggplot(cluster_plot_data_race %>% filter(plot_loc <= 5), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=race)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  facet_wrap(~cluster_id, ncol = 1) + 
  scale_fill_manual(values = color_vec, expand = c(0, 0)) +
  scale_color_manual(expand = c(0, 0)) + 
  theme_void() +
  theme(legend.position = 'none', 
        plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

# Plot map
# Note: this map is out of date; it is being workshopped below
map <- ggplot() +
  geom_sf(data = bg_data_all, aes(fill = cluster_id)) + 
  theme_void() + 
  scale_color_manual(expand = c(0, 0)) + 
  theme(legend.position = 'none',
        plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())

# Plot charts 6-10
right_plots <- ggplot(cluster_plot_data_race %>% filter(plot_loc > 5), aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=race)) +
  geom_rect() +
  coord_polar(theta="y") + # Try to remove that to understand how the chart is built initially
  xlim(c(2, 4)) + # Try to remove that to see how to make a pie chart
  facet_wrap(~cluster_id, ncol = 1) + 
  scale_fill_manual(values = color_vec) +
  scale_color_manual(expand = c(0, 0)) + 
  theme_void() + 
  theme(plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
        panel.border = element_blank(),
        panel.background = element_blank(),
        panel.grid = element_blank())
  
left_plots + map + right_plots + 
  plot_layout(widths = c(1, 4, 1))

# Generate 100 random dots, 10 per cluster
dots <- st_as_sf(sf::st_sample(clusters, size = c(10, 10, 10, 10, 10, 10, 10, 10, 10, 10), by_polygon = TRUE, type = 'random'))

# Assign cluster labels to each dot
dots <- dots %>% st_join(., st_as_sf(clusters), left = TRUE) %>%
  arrange(plot_loc) %>%
  mutate(id = row_number())

# Assign indices to synthetic population
people <- data.frame(semistratified_distribution) %>%
  left_join(., clusters %>% st_drop_geometry()) %>%
  arrange(plot_loc) %>%
  mutate(id = row_number())

# Assign people to each dot
dots <- dots %>% select(geometry, id) %>% 
  left_join(., people, by = c('id' = 'id'))
  
# ggplot() +
#   geom_sf(data = bg_data_all, aes(fill = cluster_id)) +
#   geom_sf(data = dots) +
#   theme_void() +
#   scale_color_manual(expand = c(0, 0)) +
#   theme(legend.position = 'none',
#         plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid = element_blank())

# ggplot() +
#   geom_sf(data = bg_data_all, aes(fill = cluster_id)) +
#   geom_sf(data = dots, aes(color = race, fill = race, size = median_household_income_noise, alpha = 1)) + 
#   theme_void() + 
#   scale_fill_manual(values = color_vec, name = 'Race') +
#   scale_color_manual(values = color_vec, name = 'Race') + 
#   # scale_size_continuous(range = c(0,150000)) + # , oob = scales::squish, labels = label_comma(accuracy = 1L, scale =  0.001, prefix = '$', suffix = "K") 
#   # scale_size_continuous(range = c(0, 1), labels = scales::percent) +
#   theme(legend.position = 'none',
#         plot.margin=unit(c(t=0,r=0,b=0,l=0), "pt"),
#         panel.border = element_blank(),
#         panel.background = element_blank(),
#         panel.grid = element_blank())

# Latest map
ggplot() +
  geom_sf(data = clusters, aes(fill = 'white')) +
  geom_sf(data = dots, aes(color = race, size = median_household_income_noise, fill = 'white', alpha = 1)) +
  scale_fill_manual(values = color_vec) +
  scale_color_manual(values = color_vec) + 
  theme_void()
