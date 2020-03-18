# Office
# install.packages("schrute")
library(schrute)
library(tidyverse)


# Get the Data
# From TidyT
office_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-17/office_ratings.csv')
glimpse(office_ratings)

# From Schrute
data(theoffice)

#Jim lines
# charlines <- theoffice %>%
#             filter(character == 'Jim') %>%
#             group_by(season,episode) %>%
#             summarize(nlines = length(text)) %>%
#             ungroup(season, episode) %>%
#             mutate(season = as.numeric(season),
#                    episode = as.numeric(episode))
# 
# combo <- office_ratings %>%
#   group_by(season,episode) %>%
#   left_join(charlines,by=c('season','episode'))
# combo %>%
#   ggplot(aes(x=nlines,y=imdb_rating)) +geom_point()
# 

# All lines 
charlines <- theoffice %>%
  filter(character %in% c('Michael','Jim','Pam','Dwight','Angela','Kelly')) %>% #Just picked first 6 from IMDB
  filter(!is.na(character)) %>%
  group_by(character,season,episode) %>%
  summarize(nlines = length(text)) %>%
  ungroup(character,season, episode) %>%
  mutate(season = as.numeric(season),
         episode = as.numeric(episode))

combo <- office_ratings %>%
  group_by(season,episode) %>%
  left_join(charlines,by=c('season','episode'))

po <- combo %>% 
        na.omit(character) %>%
        ggplot(aes(nlines,imdb_rating,colour=character)) +
        geom_point() +
        facet_wrap(~character) +
        stat_smooth(method = 'lm') +
        theme_classic(base_size = 12) +
        ggpomological::scale_colour_pomological('Character') +
        theme(strip.background = element_blank(),
              text = element_text(family="Courier")) +
        ylab('IMDB Rating') +
        xlab('Total number of lines') +
        labs(title = "IMDB ratings of The Office by character",
             caption = "First six characters listed on IMDB. 
             Data: schrute package and data.world")

png('TheOffice.png',width = 10,height = 5,units = 'in',res = 120)
po
dev.off()
