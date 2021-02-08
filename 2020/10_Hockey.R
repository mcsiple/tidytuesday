# 3 March 2020

game_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/game_goals.csv')

top_250 <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/top_250.csv')

season_goals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-03/season_goals.csv')

unique(game_goals$player)
unique(game_goals$season)

library(calecopal)
library(tidyverse)
library(lubridate)
library(ggrepel)
library(patchwork)

# set strings as factors to false
# options(stringsAsFactors = FALSE)


# change date to date format and add month column
game_goals <-game_goals %>%
  mutate(date = as.Date(date, format = "%yyyy-%mm-%dd")) %>%
  mutate(month = month(date))

tot_goals <- game_goals %>% 
  group_by(player,season) %>%
  summarize(tgoals = sum(goals,na.rm = T),
            tassists = sum(assists,na.rm = T))

# nice labels
MAP <- list(tgoals = "Total goals", 
            tassists = "Total assists")

totplot <- tot_goals %>%
  pivot_longer(cols = c('tgoals','tassists')) %>%
  mutate(name = recode(name,!!!MAP)) %>%
  filter(player %in% filter(top_250,raw_rank<10)$player) %>%
  ggplot(aes(x=season,y=value,colour=name)) +
  geom_line() +
  theme_minimal(base_size = 12) +
  theme(legend.position = 'bottom') +
  theme(axis.text.x = element_text(angle=45,hjust=1)) +
  scale_color_manual('',values = cal_palette("superbloom3")) +
  facet_wrap(~player) +
  ylab('') +
   xlab('Season')
  # ggtitle('Goals and assists of top-ranked players')

sdat <- game_goals %>%
  mutate(goals_home = ifelse(location == 'Home',goals,0),
         goals_away = ifelse(location == 'Away',goals,0)) %>%
  group_by(player,season) %>%
  summarize(tot_home = sum(goals_home,na.rm=T),
            tot_away = sum(goals_away,na.rm=T)) 

scatter <- sdat %>%
  ggplot(aes(x=tot_home,y=tot_away,colour=season)) +
        geom_point() +
        scale_colour_continuous('Season') +
        theme_minimal(base_size = 12) +
        xlab('Goals scored at home') +
        ylab('Goals scored away') +
        geom_label_repel(data = filter(sdat,tot_home>50 | tot_away>40),
                  aes(x=tot_home,y=tot_away,label=player),
                  xlim = c(50,NA),
                  arrow = arrow(type = "closed",
                                ends = 'last',
                                length = unit(0.02, "npc")),
                  force = 20) +
        xlim(c(0,75))

png('Hockey.png',width = 10,height=6,units = 'in',res=150)
totplot + scatter
dev.off()
