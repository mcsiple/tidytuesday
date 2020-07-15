# Coffee
library(tidyverse)
library(ggridges)
library(GGally)
library(calecopal)
library(patchwork)

coffee_ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-07-07/coffee_ratings.csv')

# Some cleanup
coffee <- coffee_ratings %>%
  mutate(country_of_origin=fct_recode(country_of_origin,"Hawai‘i" = "United States (Hawaii)",
                                      "United Republic of Tanzania"="Tanzania, United Republic Of"))

# Get the countries with more than 30 samples (for freq)
dat1 <- coffee %>%
  group_by(country_of_origin) %>%
  count() %>%
  filter(n>30) %>%
  left_join(coffee)

order_to <- dat1 %>%
  group_by(country_of_origin) %>%
  summarize(mean(acidity)) %>%
  arrange(desc(`mean(acidity)`)) %>%
  pull(country_of_origin)

dat1$country_of_origin <- factor(dat1$country_of_origin,levels = order_to) # because fct_reorder() didn't work and I do not have the time.

coffeepal <- cal_palette(name = "desert", n = 12,type = 'continuous')
coffeetheme <- hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = 'none',axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)),
        plot.title = element_text(hjust = -2),
        plot.subtitle = element_text(hjust = -2)) 

p1 <- dat1 %>%
  ggplot(aes(x=acidity,y=country_of_origin,fill=country_of_origin)) +
  ggridges::geom_density_ridges(scale = 0.95) +
  scale_fill_manual(values = coffeepal) +
  xlim(c(5,9)) +
  coffeetheme +
  xlab('Acidity grade') +
  ylab('Country of origin') +
  labs(title = "Which country of origin has the least acidic beans?",
       subtitle = "Solving a mystery that Megsie and her dad have been pondering for years!",
       caption = "Data: James LeDoux @ Buzzfeed")

p1 

coffee2 <- filter(coffee,acidity>0 & aftertaste>0 & body>0) %>% 
  rename(Acidity = acidity, Aftertaste = aftertaste,Body = body) %>%
  as.data.frame()

spcols <- cal_palette("sierra1")[c(1,4)]

p2 <- ggpairs(coffee2,
              columns = c('Acidity','Aftertaste','Body'),
              mapping = ggplot2::aes(colour=species,fill=species)) +
  scale_colour_manual(values = spcols) +
  scale_fill_manual(values = spcols) +
  coffeetheme 

png('Coffee1.png',width = 9,height = 8,units = 'in',res = 200)
p1
dev.off()

png('Coffee2.png',width = 8,height = 8,units = 'in',res = 200)
p2
dev.off()