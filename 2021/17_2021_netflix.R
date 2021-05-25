# 17_netflix
library(tidyverse)

# soundtrack: Olivia Rodrigo SOUR 

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
  filter(!is.na(genre))
alldat
 # 
 # alldat2 <- filter(alldat, add_year == 2020)
 # 
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

# nodes <- make_nodes(alldat2)
# edges <- make_edges(alldat2)
make_year_plot <- function(x){
  nodes <- make_nodes(x)
  edges <- make_edges(x)
  mygraph <- tbl_graph(nodes = nodes, edges = edges)
  
  # Make the plot
  plot <- ggraph(mygraph, layout = 'circlepack', weight = size) + 
    geom_node_circle(aes(fill = depth)) +
    labs(title = unique(x$add_year)) +
    coord_equal() +
    scale_fill_distiller() +
    hrbrthemes::theme_ipsum(base_size = 10) +
    theme(legend.position = "none")
  plot
}


# Make this into a nodes tibble
library(patchwork)
list <- alldat %>% 
  split(.$add_year) %>% 
  map(make_year_plot)
wrap_plots(list, ncol = 4, nrow = 4)
