# Tour de France

tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')
head(tdf_winners)

library(tidyverse)
library(lubridate)
library(patchwork)

# Names and life durations of TDF winners ---------------------------------

bikenames <- tdf_winners %>% 
  select(edition,winner_name,born,died,nickname,nationality,start_date) %>%
  mutate(winyear = lubridate::ymd(year(start_date), truncated = 2L))
unique(bikenames$birth_country)

bikenames$winner_name <- reorder(bikenames$winner_name, 
                                 rev(bikenames$born))
bikenames <- bikenames %>%
  mutate(lifedur = as.duration(ymd(born) %--% ymd(died))/3.154e+7) %>% #convert seconds to years at the end
  filter(!is.na(lifedur)) 

pal <- RColorBrewer::brewer.pal('Spectral',n=8)

longevity <- bikenames %>%
  ggplot() +
  geom_linerange(aes(ymin = born,ymax=died,x=winner_name,
                     colour=as.numeric(lifedur)),lwd=1.1) +
  coord_flip() +
  xlab('Winner name') +
  ylab('Year') +
  geom_point(aes(x=winner_name,y=winyear,shape=19),
             size=2,colour='darkgrey') +
  scale_shape_identity('',
                       labels = 'Won the \nTour de France',
                       breaks=c(19),
                       guide = 'legend') +
  scale_colour_gradient2('How long \nthey lived \n (years)',
                         low = pal[1], high=pal[8], midpoint = 60) +
  labs(title='The lives of Tour de France winners', 
       caption='Data: tdf package & Kaggle') +
  guides(colour = guide_legend(order = 1),
         shape = guide_legend(order = 2)) +
  hrbrthemes::theme_ft_rc() +
  theme(text=element_text(size=12,  family='Helvetica'))



# Wins by country ---------------------------------------------------------
countrywins <- bikenames %>%
  group_by(nationality) %>%
  summarize(nwins = length(winner_name))

# This is still how I reorder factors, sorry tidyverse!
countrywins$nationality <- reorder(countrywins$nationality,
                                   countrywins$nwins)

cwins <- countrywins %>%
  ggplot(aes(x=nationality,y=nwins)) +
  geom_col() +
  xlab('Winner nationality') +
  ylab('Number of wins') +
  coord_flip() +
  hrbrthemes::theme_ft_rc() +
  theme(text=element_text(size=16,family='Helvetica'))


png('TourdeFrance.png',width = 8,height = 12,units='in',res=120)
(longevity / cwins ) + plot_layout(heights = c(3,1))
dev.off()
