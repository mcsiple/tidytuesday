library(tidyverse)
library(ggplot2)
library(gganimate)
library(lubridate)

tuition_cost <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition_cost.csv')

tuition_income <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/tuition__cost_income.csv') 

salary_potential <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/salary_potential.csv')

historical_tuition <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/historical_tuition.csv')

diversity_school <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-10/diversity_school.csv')

historical_tuition2 <- historical_tuition %>%
  mutate(year1 = sub("\\-.*", "", year))

p <- historical_tuition2 %>%
  filter(!is.na(tuition_cost) & !is.na(year1)) %>%
  filter(complete.cases(.)) %>%
  mutate(year1 = as.integer(year1)) %>%
  ggplot(aes(x=type,y=tuition_cost)) +
  geom_col() +
  xlab("Institution type") +
  ylab("Tuition cost") +
  hrbrthemes::theme_ft_rc(base_size = 18) +
  labs(title = 'Tuition cost over time',
       subtitle = 'Year: {frame_time}') +
  transition_time(year1) +
  enter_fade()
p

animate(p , width = 400, height = 400)
anim_save(here::here("tuitioncost.gif"))


salary_tuition <- left_join(tuition_cost,salary_potential)

st <- salary_tuition %>% 
      filter(!is.na(early_career_pay) &
             type != 'For Profit') %>%
      ggplot(aes(in_state_total,early_career_pay,color = type)) +
      geom_point() +
      scale_colour_manual('',values = calecopal::cal_palette('chaparral2')) +
      xlab('Total in-state tuition') +
      xlim(c(10000,80000)) +
      ylim(c(10000,80000)) +
      ylab('Early-career pay') +
      hrbrthemes::theme_ft_rc(base_size = 18) +
      labs(title = 'In-state tuition vs. early-career pay',
           subtitle = 'Do graduates of more expensive instutitions earn higher salaries?',
           caption = 'Data: tuitiontracker.org')

png(filename = 'Salary_Tuition.png',width = 6,height = 6,units = 'in',res = 120)
st
dev.off()
