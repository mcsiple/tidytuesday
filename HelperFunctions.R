# Helper functions + code tidbits

# for increasing the small little font size on the {hrbrthemes} plots:
# theme_ipsum_rc() +
#   theme(axis.title.x = element_text(size = rel(1.5)),
#         axis.title.y = element_text(size = rel(1.5)))


# for increasing the number of colors in a palette:
lengthen_pal <- function(x=1:10,shortpal){
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}


# Note! -------------------------------------------------------------------
#if you use patchwork, use the & symbol to add a theme to the whole assemblage
# p1 + p2 + plot_annotation(title = 'Patchwork plots',subtitle = 'Reminder about adding a theme to the whole shebang') & theme(text = element_text('Roboto Condensed',size = 16))
