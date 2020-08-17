
library(ggstream)
library(tidyverse)
library(tidytext)
library(beyonce)
library(patchwork)
library(ggpomological)

# Get the data ------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load('2020-08-11')
avatar <- tuesdata$avatar
scenes <- tuesdata$scene_description


# Palettes ----------------------------------------------------------------
pn_pal <- c('#FFFFFF','#0096FF')
pn_pal2 <- c('darkgrey','lightblue')
char_pal <- beyonce_palette(90,n = 11)[c(1,4,10,11)]

theme_avatar <- theme_pomological_plain() +
  theme(text = element_text(family = "Herculanum", size = 12),
        legend.position = 'bottom',
        legend.box="vertical", legend.margin=margin(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank()) 
# Plot 1: sentiments in episodes ------------------------------------------
xx <- avatar %>%
  mutate(clean_char = str_replace_all(character_words, "[^a-zA-Z\\s]", " ")) %>%
  mutate(clean_char = str_trim(clean_char, side = "both"))

data('stop_words')

tidied_words <- xx %>%
  unnest_tokens(word, clean_char) %>%
  anti_join(stop_words, by = "word") #get just the 'active words' 

d1 <- tidied_words %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  group_by(book,chapter_num,sentiment) %>%
  summarize(n = length(sentiment)) %>%
  pivot_wider(names_from = sentiment,values_from = n) %>%
  mutate(overall = positive - negative) %>%
  mutate(pn = ifelse(overall>0, "Positive", "Negative"))
d1$book <- factor(d1$book,levels = c('Water','Earth','Fire'))

# Does Appa's presence mean more positive words?
appafreq <- avatar %>%
  mutate(n_appa = str_count(full_text,"Appa")) %>%
  group_by(book, chapter_num) %>%
  summarize(n_appa = sum(n_appa,na.rm=T))

# Appa appearances
d1a <- d1 %>%
  left_join(appafreq,by=c('book','chapter_num')) %>%
  mutate(fake_y = -50, fake_size = 160)
d1a$book <- factor(d1a$book,levels = c('Water','Earth','Fire'))

d1a %>% 
  ggplot(aes(x=chapter_num,y=fake_y,size=n_appa)) + 
  geom_point(colour = 'white') +
  geom_point(data = d1a,
             aes(x=chapter_num,y=fake_y,size = fake_size),
             shape =1,colour = 'white')

p1 <- d1 %>%
  ggplot(aes(x=chapter_num,y=overall)) +
  geom_hline(yintercept = 0, colour = 'darkgrey') +
  geom_segment(aes(x = chapter_num, xend=chapter_num, y=0, yend=overall,color = pn),lwd=4) +
  scale_colour_manual('',values = pn_pal2) +
  geom_point(data = d1a,
             aes(x=chapter_num,y=fake_y),
             shape = 1,size = 5, colour = 'darkgrey') +
  geom_point(data = d1a,
             aes(x = chapter_num,y = fake_y,size = n_appa),
             colour = 'darkgrey') +
  scale_size(name = "Number of Appa mentions") +
  guides(size = "legend") +
  facet_wrap(~book,ncol = 1) +
    xlab("Chapter") +
  ylab("Overall Bing sentiment score") +
  theme_avatar +
  labs(title = "Avatar: The Last Airbender",
       subtitle = "Are sentiments better when Appa is present?")

png('NewAvatar.png',width = 7,height = 10,units = 'in',res=200)
p1
dev.off()


# Plot 2: ggstream plot of character lines over time ----------------------
d2 <- avatar %>%
  mutate(character_lumped = fct_lump(character,n = 5,other_level = 'someone_else')) %>%
  group_by(book,chapter_num,character_lumped) %>%
  count() %>%
  filter(!character_lumped %in% c("Scene Description","someone_else"))
d2$book <- factor(d2$book,levels = c('Water','Earth','Fire'))

p2 <- d2 %>%
  ggplot(aes(x = chapter_num,y = n,
             fill = character_lumped)) +
  geom_stream(colour='white',size=0.1) +
  scale_fill_manual('',values = char_pal) +
  facet_wrap(~book,ncol = 1) +
  xlim(c(0,20)) +
  xlab('Chapter') +
  ylab('Number of lines') +
  hrbrthemes::theme_ft_rc() +
  theme(legend.position = 'bottom',
        axis.ticks.y = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank()) +
  labs(caption = "Data: {appa} package by Avery Robbins")
p2


# Put em together and add title etc ---------------------------------------

png("33_Airbender.png",width = 11,height = 8,units = 'in',res = 200)
p1 + p2
dev.off()


