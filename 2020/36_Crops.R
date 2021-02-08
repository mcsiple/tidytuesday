# 36: crop yields
library(tidyverse)
library(hrbrthemes)
library(calecopal)

tuesdata <- tidytuesdayR::tt_load(2020, week = 36)

# Data
key_crop_yields <- tuesdata$key_crop_yields
fertilizer <- tuesdata$cereal_crop_yield_vs_fertilizer_application
tractors <- tuesdata$cereal_yields_vs_tractor_inputs_in_agriculture
land_use <- tuesdata$land_use_vs_yield_change_in_cereal_production
arable_land <- tuesdata$arable_land_pin

# Palettes
source('HelperFunctions.R')
croppal <- lengthen_pal(x = 1:11,shortpal = cal_palette('bigsur'))

# give data nicer formatting ----------------------------------------------
clean_names <- function(x) word(x,1) %>% tolower()

d1 <- key_crop_yields %>%
  rename_with(clean_names) 


# How have yields changed over time? --------------------------------------

d1a <- d1 %>%
  group_by(year) %>%
  summarize_at(vars(wheat:bananas),.funs = sum, na.rm = TRUE) %>%
  pivot_longer(names_to = "crop",cols = wheat:bananas) 

d1b <- d1a %>%
  group_by(year) %>%
  summarize(tot_prod = sum(value,na.rm = T)) %>%
  right_join(d1a) %>%
  mutate(prop = value/tot_prod)

d1b %>% group_by(year) %>% summarize(sum(prop)) 

p1 <- d1a %>% 
  ggplot(aes(x=year,y=value,fill = crop)) +
  geom_area(color = 'white') +
  scale_fill_manual('Crop', values = croppal) +
  xlab('Year') +
  ylab('Global yield (tonnes per hectare)') +
  theme_ipsum_rc() +
  labs(title = '', #blank labels to match alginment of plots
       subtitle = '',
       caption = 'Data: Our World in Data')


# Which crops are dominant for each country? ------------------------------
# If you wanted to make a column with the max value from all yield cols
# prod_cols <- d1 %>% select(wheat:cocoa) %>% names()
# test <- d1 %>%
#   mutate(maxval = pmax(!!!rlang::syms(prod_cols),na.rm = TRUE))

d1x <- d1 %>% 
  mutate(across(everything(),
                ~replace_na(.x, 0)))

# Messy, but you know what? I don't care
d1x$dom_crop <- colnames(d1x[,-(1:3)])[max.col(d1x[,-(1:3)],ties.method="first")]
country_key <- c(`United States` = "USA",
                 `Democratic Republic of Congo` = "Democratic Republic of the Congo",
                 Congo = "Republic of Congo")
d1y <- d1x %>% 
  filter(year == 2018) %>%
  select(entity,dom_crop) %>%
  mutate(entity = recode(entity,
                         !!!country_key))

croppal2 <- croppal[c(1:4,6,8)] # first four plus maize and potatoes
bgcolor <- "white"

crop_map <- map_data("world") %>%
  left_join(d1y,by = c('region' = 'entity'))

p2 <- ggplot(crop_map) +
  geom_polygon(data = crop_map, aes(x = long, y = lat,
                                    group = group),
               fill = "grey30",
               size = 0.1) +
  geom_polygon(data = crop_map, aes(x = long, y = lat,
                                    group = group,fill = dom_crop),
               colour = "white", size = 0.1) +
  coord_map(xlim = c(-180,180)) +
  scale_fill_manual('Dominant crop (2018)',
                    values = croppal2) +
  theme_ipsum_rc() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.background = element_rect(fill = bgcolor,colour = bgcolor),
        plot.background = element_rect(fill = bgcolor,colour = bgcolor),
        panel.grid = element_blank(),
        panel.grid.major = element_blank(),
        axis.text.y = element_blank(),
        legend.position = 'none',
        plot.margin = unit(c(0,0,0,0), "lines")) +
  labs(title = 'Crop production',
       subtitle = 'Dominant stocks in each country and global yield')

p2

library(cowplot)
png('36_Crops.png',width = 12,height = 6,units = 'in',res = 200)
plot_grid(p2,p1,nrow = 1) + 
  theme(plot.background = element_rect(fill = bgcolor,colour = bgcolor))
dev.off()