library(tidyverse)


tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics
totplastics <- plastics %>% 
  group_by(country, year) %>%
  summarize_at(vars(empty:pvc), .funs = ~ sum(.x, na.rm = TRUE))

totplastics %>% 
  filter(year == 2020) %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  ggplot(aes(x = country, y = value, fill = type, colour = type)) +
  geom_col() +
  coord_polar(theta = "y")
  
