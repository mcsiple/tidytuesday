#Rap!
polls <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/polls.csv')
rankings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-14/rankings.csv')

library(tidyverse)
library(patchwork)

# Heat map showing 'golden years'
rankheat <- polls %>%
  ggplot(aes(x=year,y=rank)) +
  stat_density_2d(aes(fill = ..density..),
                  geom = "raster", contour = FALSE) +
  viridis::scale_fill_viridis('Density of tracks \n at that ranking',option = 'plasma') +
  hrbrthemes::theme_ft_rc() +
  xlab('Year') +
  ylab('Ranking')


# Subset to female artists
femrappers <- polls %>% 
  filter(gender=='female')

femfreq <- femrappers %>%
  group_by(year) %>%
  summarize(Nfem = length(title))

hg <- femfreq %>% 
  ggplot(aes(x=year,y=Nfem)) +
  geom_col(colour='white') +
  hrbrthemes::theme_ft_rc() +
  xlab('Year') +
  ylab("Number of female hip-hop\n  artists on critics' lists")

criticsummary <- polls %>% 
  group_by(critic_rols,gender) %>%
  count() %>%
  ungroup() %>%
  group_by(critic_rols) %>%
  mutate(percent = n/sum(n))

whichcritics <-  criticsummary %>% 
  ggplot(aes(x=critic_rols,y=percent,fill=gender,colour = gender)) +
  geom_col() +
  scale_fill_brewer('Artist gender',palette = 'BuPu') +
  scale_color_brewer('Artist gender',palette = 'BuPu') +
  xlab('Critic role') +
  ylab('Percent of top 5 list') +
  coord_flip() +
  hrbrthemes::theme_ft_rc() +
  theme(legend.position = 'bottom') +
  labs(title = 'Women in hip-hop top 5 lists',
       subtitle = 'Who put them on their top five, and when their tracks came out') +
  theme(plot.title = element_text(hjust = -.1),
        plot.subtitle = element_text(hjust = -.1))


png('HipHop.png',width = 13,height = 9,units = 'in',res = 120)
whichcritics + 
  ( ( hg/rankheat ) + 
      plot_layout(heights = c(1,6)) ) +
  plot_layout(widths = c(2,1))
dev.off()  


# Extras ------------------------------------------------------------------

# peryearsongs <- polls %>% 
#   group_by(year) %>% 
#   summarize(totsongs = length(title))
# 
# peryearsongs %>% 
#   ggplot(aes(x=year,y=totsongs)) + 
#   geom_line(colour='white') +
#   xlab('Year') +
#   ylab('Number of songs on BBC list') +
#   hrbrthemes::theme_ft_rc()
