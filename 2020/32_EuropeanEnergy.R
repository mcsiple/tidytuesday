# European energy
library(tidyverse)


# Get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-08-04')
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals
unique(country_totals$type)


# Map stuff ---------------------------------------------------------------
world_map <- map_data("world")
ggplot(world_map) +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
               fill = "grey30", colour = "grey45", size = 0.1) +
  theme_void()
