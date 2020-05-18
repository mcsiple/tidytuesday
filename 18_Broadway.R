library(tidyverse)
library(lubridate)
library(ggExtra)
library(patchwork)
#devtools::install_github("thebioengineer/tidytuesdayR")
#devtools::install_github("seananderson/ggsidekick")

tuesdata <- tidytuesdayR::tt_load(2020, week = 18)
grosses <- tuesdata$grosses
synopses <- tuesdata$synopses
cpi <- tuesdata$cpi
pre_1985_starts<- tuesdata$`pre-1985-starts`

# Made a function to expand a palette to length(unique(x)):
pal_lengthen <- function(x,shortpal){
  ncolours <- length(unique(x))
  newpal <- colorRampPalette(shortpal)(ncolours)
  return(newpal)
}

synopses %>% 
  filter(str_detect(synopsis, "Sondheim"))
# Already mad that there are no Sondheim classics in here... where did they go??

grosses2 <- grosses %>%
  mutate(year = year(ymd(week_ending)))

# Get densities for a contour plot
dens <- MASS::kde2d(grosses$avg_ticket_price,
                    grosses$pct_capacity, n = 50)
densf <- data.frame(expand.grid(x = dens$x, y = dens$y),
                    z = as.vector(dens$z))

# Palettes from https://carto.com/carto-colors/ + datapasta:
mint <- c("#e4f1e1", "#b4d9cc", "#89c0b6", "#63a6a0", "#448c8a", "#287274", "#0d585f")
mypal <- pal_lengthen(x = 1:9,shortpal = mint)

maxes <- grosses2 %>%
  filter(pct_capacity == max(pct_capacity) |
           avg_ticket_price == max (avg_ticket_price))


# Contour plot with scatter -----------------------------------------------
p1 <- ggplot(densf, aes(x=x,y=y,z=z)) + 
      geom_contour_filled(breaks=seq(min(densf$z),
                                     max(densf$z),
                                     length.out=10),
                          aes(colour=after_stat(level))) +
      scale_fill_manual(values = mypal) +
      scale_colour_manual(values = mypal) +
      geom_point(data = sample_n(grosses2,size = 100),
                 aes(x=avg_ticket_price,y=pct_capacity),inherit.aes = FALSE,alpha=0.5,size = 2) +
      geom_point(data = filter(grosses2,pct_capacity>1.2 | avg_ticket_price> 400),
                 aes(x=avg_ticket_price,y=pct_capacity),inherit.aes = FALSE,alpha=0.5,size = 2) +
      geom_hline(yintercept = 1,lty=2,colour='darkgrey') +
      geom_curve(aes(x=70, y=1.2, xend = maxes$avg_ticket_price[1],
                     yend=maxes$pct_capacity[1]*.99), alpha=0.5,
                 inherit.aes=FALSE,arrow=arrow(length= unit(0.015, "npc")),
                 curvature = -0.2, color='darkgrey') + #adding arrows to join text to points
      geom_curve(aes(x=400, y=.7, xend=maxes$avg_ticket_price[2],
                     yend=maxes$pct_capacity[2]*1.01), alpha=0.5,
                 inherit.aes=FALSE,arrow=arrow(length= unit(0.015, "npc")),
                 curvature = -0.2, color='darkgrey') +
      geom_label(aes(x=70,y = 1.2),label = maxes$show[1]) +
      geom_label(aes(x=400,y = .7),label = maxes$show[2]) +
      scale_x_continuous(expand = c(0.001, 0.001)) +
      scale_y_continuous(expand = c(0.001, 0.001)) +
      xlab('Average ticket price (USD)') +
      ylab('Percent capacity') +
      ggsidekick::theme_sleek(base_size = 14) +
      theme(legend.position = 'none')

top_sellers <- grosses2 %>% 
  group_by(show) %>%
  summarize(total_gross = sum(weekly_gross)) %>%
  top_n(4)

mothcols <- PNWColors::pnw_palette("Moth",4,type = 'discrete') %>% as.vector()

p2 <- grosses2 %>%
  filter(show %in% top_sellers$show & year<2020) %>%
  ggplot(aes(x=year,y=weekly_gross/1e6,colour=show,group=show)) + 
  geom_point(alpha=0.7) +
  stat_summary(fun=mean, geom="line") +
  xlab('Year') +
  ylab('Weekly gross (x 1 million USD)') +
  ggsidekick::theme_sleek(base_size = 14) +
  theme(legend.position = 'bottom') +
scale_colour_manual('',values=mothcols)
  
p3 <- ggMarginal(p2, 
                 margins = 'y',
                 groupFill = TRUE,
                 colour='darkgrey',
                 size=4) 


png(filename = 'Broadway.png',width = 12,height = 5,units = 'in',res=120)
p1 + p3
dev.off()
