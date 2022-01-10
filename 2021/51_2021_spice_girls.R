# Spice girls!

# Libraries ---------------------------------------------------------------
# remotes::install_github("hrbrmstr/ggchicklet")
library(tidyverse)
library(tidytext)
library(glue)
library(stringr)
library(ggchicklet)
library(GGally)
library(ggcorrplot)

# Color palettes and themes -----------------------------------------------
# Palettes from Spice Girls album covers, made on coolors.co
spice_pal <- c("#EF4035", "#FD7D34", "#9A351D", "#88D200", "#f9ea9a", "#cDB2CB", "#FFFFFF", "#BE525F", "#CDE009", "#A9973E") # 10 colours
spiceworld_pal <- c("#B0A7B4", "#FF2332", "#07047C", "#FF1085", "#B6E6D1", "#E8F650", "#00A8D1", "#FFB463") # 8 colours

scales::show_col(spice_pal)
scales::show_col(spiceworld_pal)

# Data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load("2021-12-14")
studio_album_tracks <- tuesdata$studio_album_tracks
lyrics <- tuesdata$lyrics

tokens <- lyrics %>%
  mutate(nsingers = str_count(section_artist, boundary("word"))) %>%
  mutate(nsingers = ifelse(nsingers > 5, 5, nsingers)) %>%
  unnest_tokens(output = "word", input = "line") %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(nsingers) %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>%
  mutate(
    prop_negative = negative / (negative + positive),
    prop_positive = positive / (negative + positive),
    pos_ratio = positive / negative
  )

tokens2 <- lyrics %>%
  unnest_tokens(output = "word", input = "line") %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(track_number, album_name, section_artist) %>%
  count(sentiment) %>% # count the # of positive & negative words
  spread(sentiment, n, fill = 0) %>%
  mutate(
    prop_negative = negative / (negative + positive),
    prop_positive = positive / (negative + positive),
    pos_ratio = positive / negative
  )

tokens %>%
  select(nsingers, pos_ratio) %>%
  pivot_longer(pos_ratio) %>%
  ggplot(aes(x = nsingers, y = value)) +
  geom_point() +
  geom_line()


# Spread singers from section_artist into columns --------------------------------------
get_singer <- function(col_str, singer) {
  ifelse(grepl(pattern = singer, x = col_str), 1, 0)
}

singers <- c("Scary", "Posh", "Ginger", "Baby", "Sporty")
# Divide singers for each lyric into their own column a la presence/absence
lyrics_wide <- lyrics %>%
  mutate(
    Baby = get_singer(singer = "Baby", section_artist),
    Posh = get_singer(singer = "Posh", section_artist),
    Scary = get_singer(singer = "Scary", section_artist),
    Sporty = get_singer(singer = "Sporty", section_artist),
    Ginger = get_singer(singer = "Ginger", section_artist),
    All = get_singer(singer = "All", section_artist)
  )
# No idea how to do this better...sorry tidyverse!
for (i in 1:nrow(lyrics_wide)) {
  lyrics_wide[i, "Baby"] <- ifelse(lyrics_wide[i, "All"] == 1, 1, lyrics_wide[i, "Baby"])
  lyrics_wide[i, "Scary"] <- ifelse(lyrics_wide[i, "All"] == 1, 1, lyrics_wide[i, "Scary"])
  lyrics_wide[i, "Sporty"] <- ifelse(lyrics_wide[i, "All"] == 1, 1, lyrics_wide[i, "Sporty"])
  lyrics_wide[i, "Posh"] <- ifelse(lyrics_wide[i, "All"] == 1, 1, lyrics_wide[i, "Posh"])
  lyrics_wide[i, "Ginger"] <- ifelse(lyrics_wide[i, "All"] == 1, 1, lyrics_wide[i, "Ginger"])
}

lyrics_wide2 <- lyrics_wide %>%
  select(-All) %>%
  unnest_tokens(output = "word", input = "line") %>%
  inner_join(get_sentiments("afinn")) %>%
  pivot_longer(cols = Baby:Ginger, values_to = "Singer") %>%
  filter(Singer == 1) %>%
  select(-Singer, -section_artist) %>%
  group_by(track_number, album_name, name) %>%
  count(sentiment) %>% # count the # of positive & negative words
  ungroup() %>%
  spread(sentiment, n, fill = 0)


# Geom_chicklet plot of sentiments ----------------------------------------

p1 <- lyrics_wide2 %>%
  mutate(negative = negative * -1) %>%
  rename(Singer = name) %>%
  ggplot(aes(x = track_number, y = positive, fill = Singer)) +
  geom_chicklet() +
  geom_chicklet(aes(x = track_number, y = negative, fill = Singer)) +
  scale_fill_manual(values = spiceworld_pal[3:7]) +
  geom_hline(yintercept = 0) +
  facet_wrap(~ factor(album_name, # order albums chronologically
    levels = c("Spice", "Spiceworld", "Forever")
  ), ncol = 3) +
  xlab("Track number") +
  ylab("Number of positive or negative words \n(AFINN sentiment score)") +
  hrbrthemes::theme_ipsum_rc() +
  labs(
    x = "Track number",
    title = "Spice Girls and Sentiments",
    subtitle = "Sentiment analysis of Spice Girls lyrics and correlations between track characteristics"
  )
p1


# Studio album tracks ----------------------------------------------------

dat2 <- studio_album_tracks %>%
  select(
    danceability, energy, loudness, liveness, valence,
    album_name
  )

p2 <- GGally::ggpairs(as.data.frame(dat2),
  columns = 1:5, aes(color = album_name, alpha = 0.8)
) +
  scale_color_manual(values = spice_pal[c(1, 8, 9)]) +
  scale_fill_manual(values = spice_pal[c(1, 8, 9)]) +
  hrbrthemes::theme_ipsum_rc()

x <- studio_album_tracks %>%
  select(speechiness:tempo)

xx <- cor_pmat(x)

p3 <- ggcorrplot(xx,
  type = "upper",
  method = "circle",
  ggtheme = hrbrthemes::theme_ipsum_rc,
  colors = c("#6D9EC1", "white", "#07047C")
) +
  labs(caption = "Spice Girls dataset from @jacquietran")

# Save plots --------------------------------------------------------------


png("SG.png", width = 12, height = 7, units = "in", res = 150)
p1 + p3 + plot_layout(ncol = 2, widths = c(2, 1))
dev.off()
