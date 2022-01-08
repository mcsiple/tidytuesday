# Employment
tuesdata <- tidytuesdayR::tt_load(2021, week = 9)
employed <- tuesdata$employed
earn <- tuesdata$earn
demo_names <- c("Men","Women","White","Black or African American", "Asian")

library(tidyverse)
table(employed$major_occupation)
table(employed$race_gender)

employed %>% 
  filter(race_gender!="TOTAL" & !industry %in% demo_names) %>% #
  group_by(industry, year, race_gender) %>% #
  summarize(employ_N = sum(employ_n)) %>%
  ungroup() %>%
  ggplot(aes(x = year, y = employ_N, color = industry, lty = race_gender)) +
  geom_line() +
  facet_wrap(~industry)
