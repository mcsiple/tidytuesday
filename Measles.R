#Measles vaccines
library(tidyverse)
library(stringr)
library(sf)
library(mapedit)
library(leaflet)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)

measles <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-25/measles.csv')

measles.summ <- measles %>%
                group_by(state,year) %>%
                summarize(mean.mmr=mean(mmr)) %>%
                filter(mean.mmr != -1) %>%
                as.data.frame()

states <- map_data("state") %>% 
  mutate(State = str_to_title(region))

test <- measles.summ %>% # normally I would do something with date but I don't wanna 
  select(state,mean.mmr) %>%
  right_join(states, by=c("state"="State"))

mdf <- as.data.frame(measles) %>% 
  mutate(lat = as.numeric(lat),
         lng = as.numeric(lng)) %>%
  filter(lng<(-70) & !is.na(lng) & !is.na(lat)) %>%
  filter(!is.na(xper) & xper>25)

mapplot <- test %>%
            ggplot(aes(long,lat)) +
            xlab("") +
            ylab("") +
            geom_polygon(aes(group=state,fill=mean.mmr)) +
            scale_fill_viridis_c("") +
            hrbrthemes::theme_ft_rc() +
            theme(axis.ticks = element_blank(),
                  panel.grid = element_blank(),
                  axis.text = element_blank()) +
            ggtitle("Mean statewide MMR vaccination rate")
mapplot

personalvax <- mapplot + 
  geom_point(data=mdf,aes(x=lng,y=lat,size=xper),
             colour='white', alpha=0.5) +
  scale_size_continuous("Percentage of students \n exempted for \n personal reasons (>25%)") +
  ggtitle("Students exempted from vaccines for personal reasons")


bars <- measles %>%
    filter(mmr != -1 & overall != -1) %>% #take out NAs
  group_by(state) %>%
  summarize(mean.personal = mean(xper,na.rm=T)) %>%
  filter(!is.na(mean.personal)) %>%
  ggplot(aes(x=state,y=mean.personal)) +
  geom_col(fill='white') +
  ylab("Mean percentage exempted\n for personal reasons") +
  xlab("State") +
  hrbrthemes::theme_ft_rc()

scatter <- measles %>%
  ggplot(aes(x=xmed,y=xper)) +
  geom_point(alpha=0.5,colour='white') +
  hrbrthemes::theme_ft_rc() +
  xlab("Exempted \nfor medical reasons (%)") +
  ylab("Exempted \nfor personal reasons (%)") +
  hrbrthemes::theme_ft_rc()

tiff("TidyFeb25.tiff",width = 10,height=4,units = 'in',res = 150)
p2 <- gridExtra::grid.arrange(bars,scatter,ncol=1)
cowplot::plot_grid(mapplot,p2,rel_widths = c(2,1))
dev.off()
