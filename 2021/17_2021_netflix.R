# 17_netflix
library(tidyverse)
library(tidygraph)
library(ggraph)

# soundtrack: Olivia Rodrigo - SOUR 
rodrigopal <- c("#8780C3","#71BDC3","#EBD4C8","#BB2D67","#EEC660")
source("HelperFunctions.R")
rodrigopal_10 <- lengthen_pal(1:10, rodrigopal)

# Load the data
tuesdata <- tidytuesdayR::tt_load(2021, week = 17)

# Clean up data
netflix <- tuesdata$netflix %>%
  mutate(add_year = as.numeric(str_sub(date_added, -5, -1))) %>%
  filter(!is.na(add_year)) %>%
  select(-cast,-date_added,-rating, -duration, -director, -cast, -description) %>%
  filter(across(country, ~ grepl('United States', .)))


alldat <- netflix %>% 
  separate(listed_in, into = c("g1","g2","g3"), fill = "right") %>%
  pivot_longer(cols = g1:g3, values_to = "genre") %>%
  filter(!is.na(genre)) %>%
  filter(add_year > 2012)

alldat



# Functions for bubble plot -----------------------------------------------

make_nodes <- function(alldat){
  unit_size <- alldat %>%
    group_by(type, genre) %>%
    summarize(size = length(show_id)) %>%
    ungroup() %>%
    rename(name = genre)
  listed_size <- unit_size %>%
    select(-type)
  
  type_size <- unit_size %>%
    group_by(type) %>%
    summarize(size = sum(size)) %>%
    rename(name = type)
  
  year_size <- tibble(name = as.character(alldat$add_year[1]),
                      size = sum(unit_size$size))
  
  nodes <- bind_rows(listed_size, type_size, year_size)
  return(nodes)
}


make_edges <- function(alldat) {
  x <- alldat %>%
    group_by(add_year, type, genre) %>%
    summarize(size = length(show_id)) %>%
    ungroup() %>%
    rename(name = genre)
  
  base <- tibble(from = as.character(unique(x$add_year)), 
                 to = unique(x$type))
  
  inner <- x %>% 
    select(from = type, to = name) %>% 
    distinct()
  
  edges <- bind_rows(base, inner) 
  return(edges)
}


make_year_plot <- function(x, pal = rodrigopal){
  nodes <- make_nodes(x)
  edges <- make_edges(x)
  mygraph <- tbl_graph(nodes = nodes, edges = edges)
  
  # Make the plot
  plot <- ggraph(mygraph, layout = 'circlepack', weight = size) + 
    geom_node_circle(aes(fill = depth), color = 'grey14', size = 0.2) +
    labs(title = unique(x$add_year)) +
    coord_equal() +
    scale_fill_gradientn(colours = pal) +
    hrbrthemes::theme_ipsum(base_size = 10) +
    scale_x_continuous(expand=c(0,0)) +
    scale_y_continuous(expand=c(0,0)) +
    labs(x = NULL, y = NULL) +
    theme(legend.position = "none",
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          axis.ticks = element_blank(),
          axis.text = element_blank(),
          axis.text.x = element_blank(),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 9))
  plot
}


# Make this into a nodes tibble
library(patchwork)
list <- alldat %>% 
  split(.$add_year) %>% 
  map(make_year_plot)
p1 <- wrap_plots(list, ncol = 3, nrow = 3) + 
  plot_annotation(caption = 'Data: Sara Stoudt / The Economist. Plot: @margaretsiple w/ code from @jakekaupp') & (hrbrthemes::theme_ipsum(base_size = 10) + theme(legend.position = "none",
                                                     panel.grid.major = element_blank(), 
                                                     panel.grid.minor = element_blank(),
                                                     panel.background = element_blank(),
                                                     axis.ticks = element_blank(),
                                                     axis.text = element_blank(),
                                                     axis.text.x = element_blank(),
                                                     axis.text.y = element_blank(),
                                                     plot.title = element_text(size = 12, hjust = 0))) 
  
p1

# Maybe just a nice time series fig now -------------------------------------
topgenres <- alldat %>% 
  group_by(genre) %>% 
  count() %>% 
  filter(!genre %in% c("Movies", "TV", "Stand", "Up", "Shows")) %>%
  ungroup() %>%
  slice_max(order_by = n, n = 10) %>% #get top 10 genres
  select(genre)

d2 <- alldat %>%
  right_join(topgenres) %>%
  group_by(add_year, type, genre) %>%
  summarize(n = length(unique(title))) %>%
  ungroup() %>%
  arrange(add_year) %>%
  mutate(cumu_n = cumsum(n)) %>%
  filter(add_year <= 2020) %>%
  rename(Genre = genre)

p2 <- d2 %>%
      ggplot(aes(x = add_year,y = cumu_n, fill = Genre)) +
      geom_area() +
      scale_fill_manual(values = rodrigopal_10) +
      facet_wrap(~type, ncol = 1) +
      labs(x = "Year added", y = "Cumulative number of listings added",
           title = 'Netflix additions in the U.S.',
           subtitle = 'Movies and TV shows added to Netflix since 2013. \nYellow bubbles are genres, nested within type (TV show or movie), \nnested within year') +
      hrbrthemes::theme_ipsum(base_size = 10) +
  theme(legend.position = "bottom")

png("NetflixA.png", width = 6, height = 10,units = 'in',res = 200 )
p2
dev.off()

png("NetflixB.png", width = 14, height = 10,units = 'in',res = 200 )
p1
dev.off()
