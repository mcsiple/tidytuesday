# 11_2021: Bechdel test
source("HelperFunctions.R")

tuesdata <- tidytuesdayR::tt_load(2021, week = 11)
bechdel <- tuesdata$raw_bechdel
movies <- tuesdata$movies

library(tidyverse)


# Do French movies pass the Bechdel test less often than others? ----------
d1 <- movies %>%
  mutate(isamerican = str_detect(string = country, 
                               pattern = "USA")) %>%
  group_by(year, clean_test, isamerican) %>%
  count() %>%
  filter(!is.na(isamerican)) %>%
  ungroup() %>%
  mutate(clean_test = recode(clean_test,
    "dubious" = "Probably doesn't pass",
    "nowomen" = "Fewer than two women",
    "notalk" = "Women don't talk to each other",
    "men" = "Women only talk about men",
    "ok" = "Passes the Bechdel test"
  )) %>%
  mutate(clean_test = factor(clean_test,
    levels = c(
      "Fewer than two women",
      "Women don't talk to each other",
      "Women only talk about men",
      "Probably doesn't pass",
      "Passes the Bechdel test"
    )
  ))

counts <- d1 %>%
  ungroup() %>%
  group_by(year, isamerican) %>%
  summarize(totalmovies = sum(n)) %>%
  ungroup()

props <- d1 %>%
  left_join(counts, by = c("year", "isamerican")) %>%
  mutate(prop_ctotal = n / totalmovies)

fpal <- c("#e63946", "#EA7F83", "#EEC5C0", "#a8dadc", "#457b9d")

p1 <- props %>%
  mutate(is_american = ifelse(isamerican, 
                            "American Films", 
                            "Everyone Else")) %>%
  ggplot(aes(
    x = year, y = prop_ctotal,
    colour = clean_test, fill = clean_test
  )) +
  geom_col() +
  facet_wrap(~is_american) +
  scale_fill_manual(values = fpal) +
  scale_colour_manual(values = fpal) +
  ylab("Proportion of movies") +
  labs(
    title = "The Bechdel test in American films and globally",
    subtitle = "How do American films compare to films from other countries?"
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(legend.position = "none") 


p2 <- d1 %>%
  mutate(is_american = ifelse(isamerican, "American Films", 
                            "Everyone Else")) %>%
  ggplot(aes(x = year, y = n, color = clean_test)) +
  facet_wrap(~is_american, scales = "free_y") +
  geom_point() +
  geom_smooth(se = FALSE) +
  scale_colour_manual("Bechdel result", values = fpal) +
  scale_fill_manual(values = fpal) +
  ylab("Number of movies") +
  labs(caption = "Plot: Margaret Siple (@margaretsiple) - Data: Bechdeltest.com API") +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    strip.background = element_blank(),
    strip.text.x = element_blank()
  )

library(patchwork)

png(filename = "CountryBechdel.png", width = 8, height = 8, units = "in", res = 200)
p1 + p2 +
  plot_layout(ncol = 1, heights = c(1, 3), guides = "collect")
dev.off()

# Bechdel passes by genre and rating --------------------------------------
ct <- movies %>%
  select(year, title, clean_test, binary, country, genre, rated) %>%
  tidyr::separate(country, into = paste0("c", 1:5)) %>%
  tidyr::separate(genre, into = paste0("g", 1:3))


genres <- ct %>%
  select(g1, g2, g3) %>%
  t() %>%
  c() %>%
  unique()
unique(movies$rated)

library(gggibbous)

d3 <- ct %>%
  select(title, binary, rated, g1, g2, g3) %>%
  filter(!is.na(g1)) %>%
  pivot_longer(cols = g1:g3, values_to = c("genre")) %>%
  select(-name) %>%
  group_by(rated, genre) %>%
  count(binary) %>%
  ungroup() %>%
  pivot_wider(names_from = binary, values_from = n) %>%
  mutate_at(c("FAIL", "PASS"), ~ replace_na(., replace = 0)) %>%
  mutate(total = FAIL + PASS) %>%
  filter(!is.na(genre) & genre != "Fi" & !rated %in% c("N/A", "Not Rated", "Unrated", "X", "TV-14", "TV-PG")) %>%
  mutate(genre = recode(genre, "Sci" = "Sci-Fi"))

totalrow <- d3 %>%
  select(-rated) %>%
  group_by(genre) %>%
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T), .names = "{.col}")) %>%
  mutate(rated = "Genre total")

d4 <- d3 %>%
  add_row(totalrow) %>%
  mutate(genre = fct_reorder(genre, PASS / total)) %>%
  mutate(rated = fct_relevel(rated, "G", "PG", "PG-13", "R", "NC-17", "Genre total"))

mooncolor <- "grey"
moonfill <- "white"
highlightmoon <- "#457b9d"

d4b <- d4 %>%
  filter(PASS / total > 0.66)

p3 <- d4 %>%
  ggplot(aes(x = rated, y = genre)) +
  geom_moon(aes(ratio = PASS / total), 
            fill = mooncolor, 
            colour = mooncolor) +
  geom_moon(aes(ratio = FAIL / total), 
            fill = moonfill, 
            right = FALSE, 
            colour = mooncolor) +
  geom_moon(data = d4b, aes(ratio = PASS / total), 
            fill = highlightmoon, 
            colour = highlightmoon) +
  geom_moon(data = d4b, aes(ratio = FAIL / total), 
            fill = NA, 
            right = FALSE, 
            colour = highlightmoon) +
  ylab("Genre") +
  xlab("Rating") +
  labs(
    title = "Across genres, fewer R-rated films pass the Bechdel",
    subtitle = "Blue crescents indicate where more than two-thirds of films pass the test.",
    caption = "Plot: Margaret Siple (@margaretsiple) - Data: Bechdeltest.com API"
  ) +
  hrbrthemes::theme_ipsum_rc() +
  theme(plot.title = element_text(hjust = 0.5), 
        plot.subtitle = element_text(colour = highlightmoon, hjust = 0.5))

png("BechdelMoons.png", width = 7, height = 10, units = "in", res = 200)
p3
dev.off()
