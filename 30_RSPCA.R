library(magrittr)
library(hrbrthemes)
library(tidyverse)
library(waffle)
library(patchwork) # for inset - maybe?
library(ghibli)

# Get the data ------------------------------------------------------------
animal_outcomes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_outcomes.csv')
animal_complaints <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-21/animal_complaints.csv')

tuesdata <- tidytuesdayR::tt_load(2020, week = 30)
brisbane_complaints <- tuesdata$brisbane_complaints
# Set up themes -----------------------------------------------------------

waffletheme <- theme_ft_rc() +
  theme(legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.border = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.margin = margin(0, 0, 0, 0, "cm"))
maptheme <-  theme_ft_rc() + 
  theme(legend.position = 'none',
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())
petcols <- ghibli::ghibli_palette(name = "YesterdayMedium",n = 7,type = 'discrete',direction = -1)

# Data for waffle plots
wafdat <- animal_outcomes %>% 
  filter(outcome %in% c("Rehomed","Reclaimed") & year == 2018) %>%
  select(-year,-outcome, - Total) %>%
  pivot_longer(-animal_type,names_to = "region")


wd <- wafdat %>% group_by(region)
region_list <- group_split(wd)

plotfun <- function(x){
  x %>%
    ggplot(aes(fill = animal_type,values = value/100)) +
    geom_waffle(n_rows = 4,size=.3,colour='white',make_proportional = T) +
    scale_fill_manual(values = petcols) +
    labs(subtitle= paste(x$region[1],"( n = ", sum(x$value),"animals )" )) +
    waffletheme +
    coord_equal()
}

act <- region_list[[1]] %>% 
  mutate(region=fct_recode(region,"Australian Capital Territory"="ACT")) %>% 
  plotfun()
nsw <- region_list[[2]] %>% 
  mutate(region=fct_recode(region,"New South Wales"="NSW")) %>% 
  plotfun() 
nt <- region_list[[3]] %>% 
  mutate(region=fct_recode(region,"Northern Territory"="NT")) %>% 
  plotfun()
qld <- region_list[[4]] %>% 
  mutate(region=fct_recode(region,"Queensland"="QLD")) %>% 
  plotfun()
sa <- region_list[[5]] %>% 
  mutate(region=fct_recode(region,"South Australia"="SA")) %>% 
  plotfun()
tas <- region_list[[6]] %>% 
  mutate(region=fct_recode(region,"Tasmania"="TAS")) %>% 
  plotfun()
vic <- region_list[[7]] %>% 
  mutate(region=fct_recode(region,"Victoria"="VIC")) %>% 
  plotfun()
wa <- region_list[[8]] %>% 
  mutate(region=fct_recode(region,"Western Australia"="WA")) %>% 
  plotfun()



# Map ---------------------------------------------------------------------
# Inset maps: https://www.r-bloggers.com/inset-maps-with-ggplot2/
library(cowplot)
library(ozmaps)
library(sf)

oz_states <- ozmaps::ozmap_states

ozmap <- ggplot(oz_states) + 
  geom_sf(fill='darkgrey',colour = 'white') + 
  coord_sf() +
  waffletheme +
  labs(title = "RSPCA Outcomes",
       subtitle = "Animals rehomed or reclaimed in 2018")

# Get standalone legend to place in bigger plot
xx <- ggplot(region_list[[8]],
             aes(region, fill = animal_type)) + 
  geom_bar(colour='white') +
  scale_fill_manual("Animal type",values = petcols) +
  theme_ft_rc(base_size = 14)
legend <- get_legend(xx)



# Save it! ----------------------------------------------------------------

png("RSPCA1_v2.png",width = 14,height = 10,units = 'in',res=200)

ggdraw() +
  draw_plot(ozmap) +
  draw_plot(act, x = .71, y = .77, width = .3, height = .3,scale = 0.9)  +
  draw_plot(nsw, x = .71, y = .47, width = .3, height = .3,scale = 0.9) +
  draw_plot(nt, x = .3, y = .77, width = .3, height = .3,scale = 0.9) +
  draw_plot(qld, x = .71, y = .65, width = .3, height = .3,scale = 0.9) +
  draw_plot(sa, x = .3, y = -.08, width = .3, height = .3,scale = 0.9) + #sa
  draw_plot(tas, x = .71, y = -.08, width = .3, height = .3,scale = 0.9) + #tas
  draw_plot(vic, x = .71, y = .1, width = .3, height = .3,scale = 0.9) + #vic
  draw_plot(wa, x = 0, y = .59, width = .3, height = .3,scale = 0.9) + #wa
  draw_plot(legend, x=-.07, y=-0.05,width = .3, height = .3,scale = 1.5) +
  theme(plot.background = element_rect(fill="#252A32", color = NA)) 

dev.off()


regionsummary <- animal_outcomes %>% 
  pivot_longer(cols = ACT:WA,names_to = "region") %>%
  group_by(year,animal_type,region,outcome) %>%
  summarize(n = sum(value)) %>%
  group_by(year,animal_type,region) 

p2 <- regionsummary %>%
  summarize(alloutcomes = sum(n)) %>%
  right_join(regionsummary) %>%
  mutate(propn = n/alloutcomes) %>%
  filter(outcome %in% c("Rehomed","Reclaimed")) %>%
  group_by(year,animal_type,region) %>%
  summarize(claim_home = sum(propn)) %>%
  ggplot(aes(year,claim_home,colour=animal_type)) +
  geom_line(lwd = .7) +
  facet_wrap(~region,ncol = 4) +
  scale_colour_manual("Animal type",values = petcols) +
  theme_ft_rc() +
    theme(axis.title.x = element_text(size = rel(1.2),colour='white'),
          axis.title.y = element_text(size = rel(1.2),colour='white'),
          axis.text.x = element_text(colour = 'white'),
          axis.text.y = element_text(colour = 'white'),
          legend.text = element_text(colour = 'white'),
          legend.title = element_text(colour = 'white'),
          strip.text = element_text(colour = 'white')
         ) +
  ylab("Proportion reclaimed or rehomed") +
  xlab("Year") +
  labs(caption = "Data: Australia RSPCA")
  
p2

png("RSPCA2.png",width = 12,height = 7,units = 'in',res=200)
p2
dev.off()
