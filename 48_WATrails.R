#48_trails
# to make the colour palette, I cloned David L Miller's gist with the usfws palettes. Thank you Dave!

library(tidyverse)
library(patchwork)
library(ggraph)

# Palettes ----------------------------------------------------------------
# From gist URL: https://gist.github.com/dill/1729bbc9ad4f915942045f96a6cfbf9d
source(here::here("USFWS-DLL","usfws_palette.R"))
source(here::here("HelperFunctions.R"))
p <- usfws_palette(n = 5,name = "dolly")
trailpal <- lengthen_pal(x = 1:12, p)

Emrld <- c("#d3f2a3", "#97e196", "#6cc08b", "#4c9b82", "#217a79", "#105965", "#074050") #from Carto colors
FeatureColors <- c("#d39c83","#e597b9","#e4f1e1","#6cc08b",Emrld[4],"#d1afe8","#85c4c9")
  #fall, flowers, mtns, nodogs, oldgrowth, ridges, water
#d1eeea,#a8dbd9,#85c4c9,#68abb8,#4f90a6,#3b738f,#2a5674
hiketheme <- hrbrthemes::theme_ft_rc() +
  theme(text = element_text(colour = 'white'),title = element_text(colour = 'white'),axis.text = element_text(colour = 'white'),strip.text = element_text(colour = 'white'),
        
        plot.background = element_rect(fill = Emrld[7]),
        panel.border = element_blank(), 
        panel.background = element_blank(), 
        panel.grid = element_blank())


# Get data ----------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-11-24')
hike_data <- tuesdata$hike_data 

# minor cleanup
hike_data <- hike_data %>% 
  mutate_at(c("gain","highpoint","rating"),.funs = as.numeric) %>%
  mutate(simple_location = sub("\\ --.*", "", location))

hdat <- hike_data %>%
  mutate(nfeatures = lengths(features)) %>%
  unnest(features) %>%
  mutate(pres = 1) %>%
  pivot_wider(names_from = features,
              values_from = pres,
              values_fill = 0,values_fn = length )

p1 <- hike_data %>% 
  ggplot(aes(x = lengths(features), y = rating)) + 
  annotate("rect", xmin = 7.5, xmax = 12.5,
           ymin = 3, ymax = 5.5,
           fill = 'white', colour = NA, alpha = 0.1) +
  geom_point(alpha = 0.5,colour = 'white') +
  geom_smooth(method = 'lm',colour = trailpal[11]) +
  ylim(c(0,6.2))+
  annotate(geom = "text", x = 10, y = 6,
           family = "Roboto Condensed Light",
           label = "Megsie-preferred \nhiking zone",
           hjust = "center",
           colour = "white") +
  xlab("Number of features") +
  ylab("Hiker rating") +
  hiketheme

p1

d1 <- hdat %>%
  group_by(simple_location) %>%
  summarize(flowers = mean(`Wildflowers/Meadows`),
            mtnviews = mean(`Mountain views`),
            oldtrees = mean(`Old growth`))

p2 <- d1 %>%
  pivot_longer(cols = flowers:oldtrees) %>%
  mutate(name = recode(name, flowers = "Flowers",
                       mtnviews = "Mountain views",
                       oldtrees = "Old-growth forest")) %>%
  ggplot(aes(x=simple_location, y=value, fill = simple_location)) +
  geom_col() +
  facet_wrap(~name) +
  scale_fill_manual(values = trailpal[-1]) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  xlab("") +
  ylab("") +
  coord_polar(clip = "off") +
  labs(title = 'Searching for the best hike in Washington State',subtitle = 'Features and ratings for hikes in Washington State') +
  hiketheme +
  theme(legend.position = 'none',
        axis.text.x = element_text(size = 9),
        axis.text.y = element_blank(),
        panel.spacing = unit(4, "lines"))





# Test out a network ------------------------------------------------------
fdat <- hdat %>%
  select(`Dogs allowed on leash`:Summits)

trythis <- fdat %>%  #woe is me
  mutate(nodogs_flowers = ifelse(`Dogs not allowed`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(nodogs_water = ifelse(`Dogs not allowed`==1 & `Waterfalls`==1,1,0)) %>%
  mutate(nodogs_mtns = ifelse(`Dogs not allowed`==1 & `Mountain views`==1,1,0)) %>%
  mutate(nodogs_oldgrowth = ifelse(`Dogs not allowed`==1 & `Old growth`==1,1,0)) %>%
  
  mutate(mtns_oldgrowth = ifelse(`Mountain views`==1 & `Old growth`==1,1,0)) %>%
  mutate(mtns_flowers = ifelse(`Mountain views`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(mtns_water = ifelse(`Mountain views`==1 & `Waterfalls`==1,1,0)) %>%
  
  mutate(water_flowers = ifelse(`Waterfalls`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(water_oldgrowth = ifelse(`Waterfalls`==1 & `Old growth`==1,1,0)) %>%
  
  mutate(fall_flowers = ifelse(`Fall foliage`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(fall_water = ifelse(`Fall foliage`==1 & `Waterfalls`==1,1,0)) %>%
  mutate(fall_flowers = ifelse(`Fall foliage`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(fall_oldgrowth = ifelse(`Fall foliage`==1 & `Old growth`==1,1,0)) %>%
  
  mutate(ridges_flowers = ifelse(`Ridges/passes`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(ridges_water = ifelse(`Ridges/passes`==1 & `Waterfalls`==1,1,0)) %>%
  mutate(ridges_flowers = ifelse(`Ridges/passes`==1 & `Wildflowers/Meadows`==1,1,0)) %>%
  mutate(ridges_oldgrowth = ifelse(`Ridges/passes`==1 & `Old growth`==1,1,0)) %>%
  
  select(nodogs_flowers:ridges_oldgrowth) %>%
  filter_all(any_vars(. != 0)) %>% # remove columns that are all zeroes
  pivot_longer(cols = nodogs_flowers:ridges_oldgrowth) %>%
  group_by(name) %>%
  summarize(nhikes = sum(value)) %>%
  separate(col = name,into = c("from","to")) %>%
  uncount(nhikes)

graph <- as_tbl_graph(trythis) %>% 
  mutate(Popularity = centrality_degree(mode = 'in'))

p3 <- ggraph(graph, layout = 'kk') + 
      geom_edge_fan2(aes(alpha = stat(index)),
                    colour = "white",alpha = 0.2,
                    show.legend = FALSE) + 
      geom_node_point(aes(color = name), size = 5) + 
      scale_colour_manual("",values = FeatureColors) +
      labs(x = NULL, y = NULL,
           caption = "Data: Washington Trails Association & TidyX") +
      hiketheme +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank())



# Save it -----------------------------------------------------------------
png('48_WATrails.png',width = 12,height = 11,units = 'in',res=200)
p2 + (p1|p3) + plot_layout(ncol = 1)
dev.off()



