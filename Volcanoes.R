library(tidyverse)
library(patchwork)

# For the map:
library(sf)
library(mapview)

# Data
volcano <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/volcano.csv')
eruptions <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/eruptions.csv')
events <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/events.csv')
#tree_rings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/tree_rings.csv')
#sulfur <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-12/sulfur.csv')



# Color stuff -------------------------------------------------------------
pal = c('#fcde9c','#faa476','#f0746e','#e34f6f','#dc3977','#b9257a','#7c1d6f')

lengthen_pal <- function(x,shortpal){
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}

newpal <- lengthen_pal(x=1:10,shortpal = pal)


# Data stuff --------------------------------------------------------------
locs <- volcano %>%
  select(volcano_name,major_rock_1,latitude,longitude)

locations_sf <- st_as_sf(locs, coords = c("longitude", "latitude"), crs = 4326)
mapviewOptions(basemaps = "CartoDB.DarkMatter")
p <- mapview(locations_sf,
             zcol="major_rock_1",
             alpha=0,
             col.regions=newpal)

mapshot(p,file='volcanoplot2.pdf',
        remove_controls = c("homeButton", "layersControl"),
        res=400,
        selfcontained = FALSE,
        zoom=1)

volsumm <- eruptions %>%
  group_by(volcano_name) %>%
  count() %>%
  left_join(volcano)

palitra <- c('#0099ff','#9d98f6','#d59de5','#f4a9d3','#ffbcca','#ffd2cd','#ffe8dd')

p1 <- volsumm %>%
  ggplot(aes(x=longitude,y=n,colour = evidence_category)) +
  geom_point() +
  scale_colour_manual('Evidence category',values = palitra[c(2,3,4,5,6)]) +
  geom_vline(xintercept = 47.6062,colour='white',lty=2) + # longitude of Seattle
  ylab('Number of eruptions') +
  xlab('Longitude') +
  hrbrthemes::theme_ft_rc(base_size=11) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  geom_curve(aes(x = 100, y = 200, xend = 47.6, yend = 225),colour='white',arrow = arrow(length = unit(0.03, "npc"))) +
  annotate('text',x=100,y=180,label="Seattle",colour='white')+
  labs(title="A landscape of fear!",
       subtitle = "Was my childhood fear of volcanic annihilation well-founded?")

p1

volareas <- volcano %>%
  left_join(events) %>%
  mutate(longbin = case_when(longitude < (-100) ~ "l1",
                             longitude > (-100) & longitude < 0 ~ "l2",
                             longitude > 0 & longitude < 100 ~ "l3",
                             longitude > 100  ~ "l4"))

p2 <- volareas %>%
  mutate(longbin2 = recode(longbin,l1='< 100',l2='-100 < 0',l3='0 < x < 100',l4='x > 100')) %>%
  group_by(longbin2,event_type) %>%
  count() %>%
  filter(!is.na(event_type) & event_type != 'VEI (Explosivity Index)') %>%
  ggplot(aes(x=fct_reorder(event_type,n),y=n)) +
  geom_col(fill='lightgrey',colour='lightgrey') +
  ylab("Count") +
  xlab("Type of event") +
  facet_wrap(~longbin2,nrow=1) +
  coord_flip() +
  hrbrthemes::theme_ft_rc(base_size=10) +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(caption = 'Data: The Smithsonian Institution')

png('Volcano.png',width = 12,height = 12,units = 'in',res = 400)
p1 + p2 + plot_layout(ncol=1,heights = c(1,3))
dev.off()

