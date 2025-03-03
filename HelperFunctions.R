# Helper functions + code tidbits

# for increasing the small little font size on the {hrbrthemes} plots:
# theme_ipsum_rc() +
#   theme(axis.title.x = element_text(size = rel(1.5)),
#         axis.title.y = element_text(size = rel(1.5)))
# This is a test to see if GH is working

# for increasing the number of colors in a palette:
lengthen_pal <- function(x=1:10,shortpal){
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}


# PATCHWORK NOTE -------------------------------------------------------------------
#if you use patchwork, use the & symbol to add a theme to the whole assemblage
# p1 + p2 + plot_annotation(title = 'Patchwork plots',subtitle = 'Reminder about adding a theme to the whole shebang') & theme(text = element_text('Roboto Condensed',size = 16))


# MAP NOTES ---------------------------------------------------------------
# world_map <- map_data("world")
# ggplot(world_map) +
#   geom_polygon(data = world_map, aes(x = long, y = lat, group = group), 
#                fill = "grey30", colour = "grey45", size = 0.1) 


# Add how to print palette in graph window --------------------------------
#taken from stackoverflow
show_pal <- function(pal){ 
  plot(seq_len(length(pal)), rep_len(1, length(pal)),
     col = pal, pch = 16, cex = 3, xaxt = 'n', yaxt = 'n', xlab = '', ylab = '')
}
