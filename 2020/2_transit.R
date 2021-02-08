# 2_Transit


# Soundtrack --------------------------------------------------------------
#Swedish procedural drama my folks are watching


# Packages ----------------------------------------------------------------
# devtools::install_github("davidsjoberg/ggbump")
# install.packages("countrycode") # for merging iso2 country codes

library(tidyverse)
library(countrycode)
library(ggbump)
library(gghighlight)

# Theme -------------------------------------------------------------------
theme_transit <- hrbrthemes::theme_ft_rc() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    legend.position = "none"
  )




# Data --------------------------------------------------------------------
tuesdata <- tidytuesdayR::tt_load(2021, week = 2)
transit_cost <- tuesdata$transit_cost

tdat <- countrycode::codelist %>%
  select(country.name.en, iso2c, continent) %>%
  right_join(transit_cost, by = c("iso2c" = "country"))


# Summarize data
cont_lengths <- tdat %>%
  group_by(continent, year) %>%
  summarize(tot_length = sum(length, na.rm = T)) %>%
  drop_na() %>%
  group_by(year) %>%
  mutate(rank = dense_rank(desc(tot_length)))


# ggbump plot of total track length ---------------------------------------

p1 <- cont_lengths %>%
  filter(year < 2021) %>%
  ggplot(aes(year, rank, color = continent, group = continent)) +
  geom_bump(smooth = 10, size = 1.5, lineend = "round") +
  scale_y_reverse() +
  ghibli::scale_colour_ghibli_d(name = "MononokeMedium", direction = -1) +
  gghighlight(unhighlighted_params = list(
    size = 1,
    colour = alpha("grey", 0.2)
  )) +
  facet_wrap(~continent, ncol = 1)

p1 +theme_transit


# Density plot of number of stations per track/numbers of years --------

p2 <- tdat %>%
  filter(!is.na(start_year)) %>%
  filter(!is.na(end_year)) %>%
  filter(!is.na(continent)) %>%
  filter(end_year != "X") %>%
  mutate_at(c("start_year", "end_year"), .funs = as.numeric) %>%
  mutate(nyears = end_year - start_year) %>%
  ggplot(aes(x = stations / nyears, color = continent, fill = continent)) +
  # coord_flip() +
  geom_density() +
  xlab("Number of stations built per year") +
  ghibli::scale_colour_ghibli_d(name = "MononokeMedium", direction = -1) +
  scale_fill_manual(values = alpha(ghibli::ghibli_palette(name = "MononokeMedium", direction = -1),
                                   alpha = 0.5)) +
  facet_wrap(~continent, ncol = 1, scales = "free_y")

p2 + theme_transit

# Put them together -------------------------------------------------------
library(patchwork)

png("2_Transit.png",width = 11, height = 9, units = 'in',res = 200)
p1 + p2 +
  plot_annotation(
  title = "Transit",
  subtitle = "Which continents had the longest rail projects, and how long did they take?",
  caption = "Data: Transit Costs Project"
) & theme_transit
dev.off()
