# 29: Astronauts
library(tidyverse)
library(hrbrthemes)
library(vegan)
library(beyonce)
library(patchwork)

source('HelperFunctions.R')

astronauts <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-14/astronauts.csv')

# smidge of cleanup
astronauts  <- astronauts %>%
  mutate(occupation = tolower(occupation))

# set theme and colours
spacetheme <- theme_ft_rc() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
    theme(axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          legend.position = 'bottom')
palitra <- c('#FFFFDE','#FFE9B3','#FFCC94','#FFA98C','#FF809F','#FF5AC8','#FF52FF')
pal <- rev(lengthen_pal(x = 1:8,shortpal = beyonce_palette(101)))
gpal <- beyonce_palette(48)[c(4,6)]

p1 <- astronauts %>%
  filter(eva_hrs_mission>0) %>%
  ggplot(aes(x=year_of_mission,y=eva_hrs_mission,colour=sex)) + 
  geom_point(alpha=0.5) +
  geom_smooth(method = 'lm') +
  xlab('Year of mission') +
  ylab('Extravehicular hours') +
  scale_colour_manual('',values = gpal) +
  theme(legend.position = 'bottom')

dtable <- astronauts %>% # diversity table
  count(mission_title,year_of_mission,nationality) %>%
  pivot_wider(names_from = nationality,
              values_from = n,
              values_fill = 0) 

divs <- data.frame(shannon = diversity(dtable[,-c(1,2)]),
                   dtable[,c(1,2)])

missions <- astronauts %>%
  distinct(mission_title,year_of_mission) %>%
  full_join(divs)

p2 <- missions %>%
  group_by(year_of_mission,shannon) %>%
  summarize(nmissions = length(mission_title)) %>%
  filter(shannon>0) %>%
  ggplot(aes(x = year_of_mission,y = shannon,size = nmissions)) +
  geom_point(colour = 'lightblue',alpha=0.5) +
  xlim(c(1960,2020)) +
  xlab("Year") +
  ylab("Diversity index of astronaut nationalities") +
  labs(size = "Number of missions")


singlecountrymissions <- missions %>%
  filter(shannon == 0) %>%
  left_join(astronauts %>% select(nationality,mission_title)) %>%
  mutate(natcat = fct_lump(nationality,n = 6)) %>%
  group_by(year_of_mission,natcat) %>%
  summarise(n = n())

 p3 <- singlecountrymissions %>%
  ggplot(aes(x=year_of_mission,y=n,fill=natcat,colour=natcat)) +
  geom_col() +
  xlim(c(1960,2020)) +
  scale_fill_manual('Country',values=pal) +
  scale_colour_manual('Country',values=pal) +
  xlab('Year') +
  ylab('Single-country missions') +
  theme_ft_rc() 
  
png("29_astronauts.png",width = 12,height = 10,units = 'in',res = 200)
((p2 / p3 + plot_layout(heights = c(2,1),ncol=1)) | p1 ) +
  plot_layout(ncol = 2) + 
  plot_annotation(title = 'The diversity of space missions',
                  subtitle = 'Collaborative missions between countries, and gender in astronaut extravehicular hours over time',
                  caption = 'Data: Mariya Stavnichuk and Tatsuya Corlett') & spacetheme
dev.off()
