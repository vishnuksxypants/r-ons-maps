# map some ons data

library(tidyverse)
library(sf)
library(classInt)

raw_data <- read_csv("gdhi_per_head_LAU1.csv")

mean_GDHI <- mean(raw_data$`2016`)

#ultra generalised
#sf_gb <- st_read("https://opendata.arcgis.com/datasets/3dc07a60f46b4e01ab0ec8ba71c7a879_4.geojson")

#super generalised
sf_gb <- st_read("https://opendata.arcgis.com/datasets/3dc07a60f46b4e01ab0ec8ba71c7a879_3.geojson")


plot(st_geometry(sf_gb))

plot_data <- left_join(raw_data %>% select(`LAU1 code`, Region, GDHI_per_household = `2016`), sf_gb, by = c("LAU1 code" = "lau118cd"))

ggplot(data = plot_data %>% filter(Region == "London")) +
  geom_sf(aes(fill = GDHI_per_household)) +
  scale_fill_gradient2(low = "red",
                       mid = "blue",
                       high = "green",
                       midpoint = median(raw_data$`2016`),
                       labels = scales::comma) +
  coord_sf(crs = st_crs(4326))

classes <- classIntervals(plot_data$GDHI_per_household, n = 5, style = "jenks")

plot_data <- plot_data %>%
  mutate(bracket = cut(GDHI_per_household, classes$brks, include.lowest = T))

ggplot(data = plot_data) +
  geom_sf(aes(fill = bracket)) +
  scale_fill_discrete(labels = scales::comma) +
  theme(legend.position = "bottom") +
  coord_sf(crs = st_crs(4326))
