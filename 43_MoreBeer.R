# Week 43

library(tidyverse)
library(ggbeeswarm)
library(hrbrthemes)
library(calecopal)
library(patchwork)


tuesdata <- tidytuesdayR::tt_load(2020, week = 43)
states_regions <- data.frame("state" = state.abb,
                             "region" = state.region)

beer_awards <- tuesdata$beer_awards

# Cleanup on aisle b
beer_awards$state[which(beer_awards$state == 'Ak')] <- "AK"
beer_awards$state[which(beer_awards$state == 'wa')] <- "WA"

beer_counts <- beer_awards %>%
  group_by(medal, state, year) %>%
  count() %>%
  ungroup() %>%
  group_by(state, medal) %>%
  summarize(mean_nawards = mean(n,na.rm = T)) %>%
  left_join(states_regions) 

beer_counts$region[which(beer_counts$state == 'DC')] <- "South"



# Plot 1: Beeswarm plot of medals by state --------------------------------

medal_pal <- cal_palette("chaparral1")[c(4,1,2)]

maxlabs <- beer_counts %>% 
  group_by(region) %>%
  filter(mean_nawards == max(mean_nawards) & !is.na(region))

p1 <- beer_counts %>%
  mutate(region = fct_relevel(region,"West","North Central","South","Northeast")) %>%
  ggplot(aes(x = region,y = mean_nawards,colour = medal)) +
  geom_quasirandom(method = "tukeyDense",size = 2) +
  xlab("Region") +
  ylab("Mean number of awards") +
  scale_colour_manual("Medal", values = medal_pal) +
  coord_flip() +
  geom_text_repel(data = maxlabs,
                  aes(label = state),
                  force = 10, nudge_x = 0.2,show.legend = FALSE) 
p1


dat2 <- beer_awards %>%
  left_join(states_regions) 

# DC is a state now, and MD and VA are both "South" so...!
dat2$region[which(dat2$state == "DC")] <- "South"

dat3 <- dat2 %>%
  group_by(region,year,medal) %>%
  count()

p2 <- dat3 %>% 
  ggplot(aes(x=year,y=n,fill = medal)) +
  geom_area(lwd=0.3, colour = 'white') +
  facet_wrap(~region, scales = "free_y",ncol = 1) +
  scale_fill_manual("Medal",values = medal_pal) +
  theme(legend.position = 'none') +
  xlab("Year") +
  ylab("Number of states with awards") +
guides(fill = FALSE) 
  #+
#  guides(colour = FALSE)

beertheme <- theme_ipsum_rc(base_size = 12) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        plot.margin=unit(c(3,3,3,3),"mm"))

png("43_MoreBeer.png",width = 10,height = 8,units = "in",res = 200)
p1 + p2 + 
  plot_layout(ncol = 2, guides = 'collect') + 
  plot_annotation(title = "Beer awards",
                  subtitle = "Which regions of the U.S. cleaned up at the Great American Beer Festival?",
                  caption = "Data: The Great American Beer Festival") & beertheme 
dev.off()
