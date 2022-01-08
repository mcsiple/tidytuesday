# Week 23: SURVIVOR the TV show

library(tidyverse)

summary <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/summary.csv')
challenges <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/challenges.csv')
castaways <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/castaways.csv')
viewers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/viewers.csv')
jury_votes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-06-01/jury_votes.csv')

head(summary)

summary2 <- summary %>%
  mutate(filming_duration = filming_ended - filming_started)
# summary3 <- summary2 %>%
#   pivot_longer(cols = premiered:filming_ended, values_to = "date")
# summary3 %>%
#   ggplot(aes(x = season, y = date, colour = name)) +
#   geom_point()

summary %>%
  ggplot(aes(x = season)) +
  geom_linerange(aes(ymin = filming_ended, ymax = premiered), lwd=3) +
  coord_flip()



summary2 %>%
  ggplot(aes(x=viewers_premier,y=viewers_finale)) +
  geom_point()
