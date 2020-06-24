# 26 - caribou
library(tidyverse)
library(patchwork)


individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

predated <- individuals %>% 
  filter(stringr::str_detect(death_cause, 'predation')) %>%
  mutate(predator = ifelse(str_detect(death_cause,'wolf'),'wolf',
                           ifelse(str_detect(death_cause,'bear'),'bear',
                                  'unknown')))
loc_pred <- locations %>% 
  filter(animal_id %in% predated$animal_id)
unique(loc_pred$animal_id)


# Tracks without map ------------------------------------------------------

tagged_day_summary <-  locations %>%
  group_by(animal_id) %>%
  summarize(last_dttm = max(timestamp)) %>%
  right_join(locations) %>%
  group_by(animal_id) %>%
  mutate(day_of_life = as.numeric((last_dttm - timestamp)/(60*60*24))) %>%
  summarize(days_tagged = max(day_of_life))

xx <- locations %>%
  group_by(animal_id) %>%
  summarize(last_dttm = max(timestamp)) %>%
  right_join(locations) %>%
  filter(timestamp == last_dttm) %>%
  left_join(tagged_day_summary) %>%
  mutate(killed_by_preds = ifelse(animal_id %in% predated$animal_id,"Killed by predators","Other cause of death or tag loss")) %>%
  rename(`Number of days tagged` = days_tagged)

p1 <- xx %>%
  arrange(desc(killed_by_preds)) %>%
  ggplot(aes(x=longitude,y=latitude,
             size = `Number of days tagged`,
             colour = killed_by_preds)) +
  geom_point(alpha=0.7) +
  scale_color_manual("",values=c('#F072B6','white')) +
  guides(size=guide_legend(override.aes=list(colour="white"),
                           title.position = "top",
                           ncol = 1),
         colour = guide_legend(ncol = 1))+
  xlab('Longitude') +
  ylab('Latitude') +
  hrbrthemes::theme_ft_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        legend.position = 'bottom') +
  labs(title = 'The work of wolves',
       subtitle = "Tagged caribou eaten by wolves on unceded First Nations territory")

p1  


pdat2 <- loc_pred %>%
  group_by(animal_id) %>%
  summarize(last_dttm = max(timestamp)) %>%
  right_join(loc_pred) %>%
  group_by(animal_id) %>%
  mutate(day_of_life = as.numeric((last_dttm - timestamp)/(60*60*24))) %>%
  mutate(prop_of_tagged_time = day_of_life/(max(day_of_life))) 

ends <- loc_pred %>%
  group_by(animal_id) %>%
  mutate(last_dttm = max(timestamp)) %>% 
  distinct(animal_id, .keep_all=TRUE)

p2 <- pdat2  %>%
  ggplot(aes(x = longitude, y = latitude, group = animal_id,colour = prop_of_tagged_time)) +
  geom_path(alpha=0.5,lwd=1.2) +
  scale_colour_gradient("Proximity to mortality \n (fraction of tagged time)", low = '#FFF886',high = '#F072B6') +
  geom_point(data = ends,aes(x=longitude,y=latitude),colour="white",size = 2.5) +
  xlab('Longitude') +
  ylab('Latitude') +
  hrbrthemes::theme_ft_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        legend.position = 'bottom',
        legend.key.width=unit(1,"cm")) +
  labs(caption = "Data: B.C. Ministry of Environment & Climate Change")


p2

png("Caribou.png",width = 12,height = 9,units = 'in',res = 120)
p1+p2
dev.off()



# A bunch of mapping stuff that didn't work -------------------------------

# Get map (thank you Nyssa!!!)
# x <- getData(name = "GADM", country = "CAN",level = 1)
# bc <- x [x$NAME_1 == "British Columbia",]
# bc@data$id <- rownames(bc@data)
# create a data.frame from our spatial object
# bcdf <- fortify(bc, region = "id")
# 
# p1 <- ggplot(bcdf, aes(x = long,y = lat)) +
#       geom_polygon(fill = "grey30", size = 0.1) +
#       geom_path(data = loc_pred, aes(x = longitude, y = latitude, group = animal_id,colour = animal_id)) +
#       theme_classic() +
#       theme(legend.position='none')
# p1

