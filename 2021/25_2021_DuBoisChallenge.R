library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2021, week = 25)
tweets <- tuesdata$tweets

world_map <- map_data("world")
map_plot <- ggplot() +
  geom_polygon(data = world_map, 
               aes(x = long, y = lat, group = group),
               fill = "grey30", colour = "grey45", size = 0.1) + 
  xlab('') +
  ylab('') +
  coord_fixed() +
  hrbrthemes::theme_ft_rc(base_size=14) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank())

map_plot +
  geom_point(data = tweets,
             aes(x = long, y = lat, size = retweet_count))
