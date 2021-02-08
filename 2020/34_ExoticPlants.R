#34_ExoticPlants
library(tidyverse)
library(ggchicklet)

source('HelperFunctions.R')

tuesdata <- tidytuesdayR::tt_load(2020, week = 34)

plants <- tuesdata$plants
actions <- tuesdata$actions
threats <- tuesdata$threats

extrafont::loadfonts()


# Palettes ----------------------------------------------------------------
dangerpal <- calecopal::cal_palette(name = 'superbloom3')[1:2]
threatpal <- calecopal::cal_palette(name = 'superbloom2') %>% 
  lengthen_pal(x = 1:12)
bgcolor <- "#FFFFF8"
countrycolor <- "#dfdacd"

world_map <- map_data("world")

plant_statuses <- plants %>% 
  select(country, group, red_list_category) %>%
  group_by(country,red_list_category) %>%
  count() %>%
  right_join(world_map,by = c("country" = "region") ) %>%
  filter(!is.na(red_list_category))


p1 <- ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group),
               fill = countrycolor, colour = "grey45", size = 0.1) +
  geom_polygon(data = plant_statuses,
               aes(x = long, y = lat, group = group,fill = log(n)), 
               colour = "grey45", size = 0.1) +
  facet_wrap(~red_list_category, ncol = 2) +
  theme_void(base_family = 'Lato') +
  scale_fill_stepsn('Number of \nspecies \n(log-scale)',colours = dangerpal) +
  theme(#panel.border = element_rect(fill = bgcolor,colour = bgcolor),
    panel.background = element_rect(fill = bgcolor,colour = bgcolor),
        plot.background = element_rect(fill = bgcolor,colour = bgcolor),
        panel.grid = element_blank(),
        legend.position = 'right') 
p1


# Chicklet plots ----------------------------------------------------------
d2 <- threats %>%
  group_by(red_list_category,continent,threat_type) %>%
  summarize(nthreatened = sum(threatened,na.rm = TRUE))

p2 <- d2 %>% 
  ggplot(aes(continent, nthreatened,fill = threat_type)) +
  geom_chicklet(width = 0.75,colour = bgcolor) +
  scale_fill_manual("Threat type",values = threatpal) +
  xlab("Continent") +
  ylab("Number of species") +
  theme(text = element_text(family = 'Lato'),
        panel.background = element_rect(fill = bgcolor,colour = bgcolor),
        plot.background = element_rect(fill = bgcolor,colour = bgcolor),
        #panel.border = element_rect(fill = bgcolor,colour = bgcolor),
        legend.background = element_rect(fill = bgcolor, colour = bgcolor),
        legend.key = element_rect(fill = bgcolor, color = NA),
        panel.grid = element_blank(),
        strip.background = element_blank(),
        legend.position = 'bottom') +
  facet_wrap(~red_list_category,ncol = 2,scales = "free_x") +
  coord_flip()
p2


library(patchwork)

all <- p1 + p2 + 
  plot_layout(heights = c(3,1)) + 
  plot_annotation(title = 'Plants in danger',
                  subtitle = 'Threats to plants that are extinct or extinct in the wild',
                  caption = 'Data: Florent Laverne & Cédric Scherer') & 
  theme(text = element_text('Lato',size = 14),
        panel.background = element_rect(fill = bgcolor,colour = bgcolor),
        plot.background = element_rect(fill = bgcolor,colour = bgcolor))

png('Plants.png',width = 12,height = 9,units = 'in',res = 200)
all
dev.off()



# cowplot option (avoid weird projection issues from axis align wi --------
library(cowplot)
p1 <- p1 + 
  coord_map(xlim=c(-180,180)) + # fix projection (thanks Dan)
  theme(plot.margin = unit(c(0,0,0,6), "lines")) + # add whitespace to the left to sort of align
  labs(title = 'Plants in danger',
       subtitle = 'Threats to plants that are extinct or extinct in the wild')
p2 <- p2 + 
  labs(caption = 'Data: Florent Laverne & Cédric Scherer')

png('Plants_cowplot.png',width = 12,height = 9,units = 'in',res = 200)
plot_grid(p1,p2,ncol = 1) + 
  theme(plot.background = element_rect(fill = bgcolor,colour = bgcolor))
dev.off()                                                                                                         
