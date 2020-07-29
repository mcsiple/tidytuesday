library(tidyverse)
library(ghibli)
library(cowplot)
library(dplyr)
library(readr)


tuesdata <- tidytuesdayR::tt_load(2020, week = 31)
penguins <- tuesdata$penguins
penguins <- penguins %>%
  filter(!is.na(sex))

# Raincloud stuff ---------------------------------------------------------
packages <- c("cowplot", "readr", "ggplot2" ,
              "dplyr", "lavaan")


# Palettes and theme ------------------------------------------------------
penpal <- ghibli_palette("SpiritedMedium",direction = -1)
pentheme <-  theme(axis.title.x = element_text(size = rel(1.5)),
                   axis.title.y = element_text(size = rel(1.5)),
                   panel.grid.major = element_blank(),
                   panel.grid.minor = element_blank())


# Plot stuff --------------------------------------------------------------

p1 <- ggplot(penguins, aes(x = sex, y = body_mass_g, fill = species)) +
  geom_flat_violin(position = position_nudge(x = .1, y = 0), 
                   adjust = 1.5, trim = FALSE, alpha = .5, colour = NA) +
  geom_point(aes(x = sex, y = body_mass_g, colour = species),
             position = position_jitter(width = .05), size = 1, shape = 20) +
  geom_boxplot(aes(x = sex, y = body_mass_g, fill = species),
               outlier.shape = NA, alpha = .5, width = .1, colour = "black") +
  scale_colour_manual("Species",values = penpal) +
  scale_fill_manual("Species",values = penpal) +
  xlab("Sex") +
  ylab("Body mass (g)") +
  #scale_y_discrete(breaks = NULL) + # remove x grid lines
  coord_flip() +
  facet_wrap(~island,scale = "free_x",ncol = 1) +
  hrbrthemes::theme_ipsum_rc() +
    theme(axis.title.x = element_text(size = rel(1.5)),
          axis.title.y = element_text(size = rel(1.5)),
          legend.position = 'none',
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank())
  
p1

p2 <- penguins %>%
  ggplot(aes(x = bill_length_mm,y = bill_depth_mm,colour = species)) +
  geom_point() +
  scale_colour_manual("Species",values = penpal) +
  facet_grid(island~sex) +
  xlab("Bill length (mm)") +
  ylab("Bill depth (mm)") +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'none')

p3 <- penguins %>%
  ggplot(aes(x = body_mass_g,y = flipper_length_mm,colour = species)) +
  geom_point() +
  scale_colour_manual("Species",values = penpal) +
  facet_grid(island~sex) +
  xlab("Body mass (g)") +
  ylab("Flipper length (mm)") +
  hrbrthemes::theme_ipsum_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = 'bottom')

png('Penguins.png',width = 12,height = 12,units = 'in',res=200)
p1 + (p2/p3 + plot_layout(heights = c(4,3))) +
  plot_annotation(title = 'Palmer penguins',
                  subtitle = 'Body mass and sex across species and island',
                  caption = "Data: Dr. Kristen Gorman (via the palmerpenguins package)") &  theme(text = element_text('Roboto Condensed',size = 16))
  dev.off()

  