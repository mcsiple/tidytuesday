#19_bestsellers

# packages ----------------------------------------------------------------
library(tidyverse)
#devtools::install_github("BlakeRMills/MetBrewer")
library(MetBrewer)
library(ggrepel)
library(hrbrthemes)



# Read in the data manually
nyt_titles_raw <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
# Clean out the ! and ? from authors
nyt_titles <- nyt_titles_raw %>% 
  mutate(author = str_replace(author, pattern = "[[:punct:]] by ","")) 

nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

# Goodreads books from here: https://www.kaggle.com/datasets/jealousleopard/goodreadsbooks?resource=download
gr <- read.csv("books.csv") #This dataset only goes to 2020 so I think it actually matches well with the NYT list
gr2 <- gr %>% 
  mutate(title = toupper(title))

colnames(gr2)
colnames(nyt_titles)

# All the titles
dat <- nyt_titles %>% 
  left_join(distinct(gr2,title,.keep_all=T), by = c("title")) %>% # don't match with duplicated titles in goodreads
  mutate_at(c('average_rating','num_pages','text_reviews_count'),as.numeric)

#Titles with both goodreads and NYT metrics
dat_matches <- dat %>% 
  filter(!is.na(bookID)) 

baseplot <- ggplot(dat_matches, 
                   aes(x=total_weeks, y=average_rating,
                       color = language_code)) + 
  geom_point(alpha = 0.7) +
  xlab("Weeks on NYT Bestseller list") +
  ylab("Average rating on Goodreads") +
  scale_color_met_d(name = 'Benedictus') +
  hrbrthemes::theme_ft_rc()

manyweeks <- dat_matches %>% 
  filter(total_weeks>120) %>%
  mutate(title = stringr::str_to_title(title))

highratings <- dat_matches %>%
  filter(average_rating>4.4)%>%
  mutate(title = stringr::str_to_title(title))

p1 <- baseplot + 
  geom_point(data=manyweeks, 
             aes(x=total_weeks, y=average_rating))+
  ggrepel::geom_text_repel(data=manyweeks, 
                           aes(x=total_weeks, y=average_rating, 
                               label = title), size = 3,
                           nudge_y = ifelse(manyweeks$title == "The Da Vinci Code", .2,-.2)
                           ) +
  geom_point(data=highratings, 
             aes(x=total_weeks, y=average_rating))+
  ggrepel::geom_text_repel(data=highratings, 
                           aes(x=total_weeks, y=average_rating, 
                               label = title), size = 3,
                           nudge_y = .1
  ) +
  guides(color = guide_legend(override.aes = aes(label = ""),title = "Language"))
  
p1

bestselling_authors <- nyt_titles %>% 
  group_by(author) %>% 
  count() %>%
  ungroup() %>%
  slice_max(n, n=10, with_ties = FALSE)

# How many books did each of the bestselling authors have on the list each year? 
bsauthors_ts <- nyt_full %>%
  filter(author %in% bestselling_authors$author) %>%
  group_by(author,year) %>%
  distinct(title) %>%
  count()

# If available, what were the average goodreads ratings of those books?
bestsellers_gr <- nyt_full %>%
  filter(author %in% bestselling_authors$author) %>%
  distinct(title) %>%
  left_join(distinct(gr2,title,.keep_all=T), by = 'title') %>%
  mutate(year = as.numeric(str_sub(publication_date, start= -4)),
         average_rating = as.numeric(average_rating)) %>%
  filter(!is.na(bookID)) %>%
  group_by(authors, year) %>%
  summarize(mean_rating = mean(average_rating,na.rm=TRUE)) %>%
  ungroup()

p2 <- bsauthors_ts %>%
  left_join(bestsellers_gr, by = c("author"="authors", "year")) %>%
  ggplot(aes(x=year,y=n)) + 
  geom_line(colour = 'white') +
  geom_point(aes(x=year,y=n,size = mean_rating),
             alpha=0.5,colour = 'lavender') +
  xlab("Year") +
  ylab("Unique NYT bestselling books") +
  facet_wrap(~author,ncol = 2) +
  guides(size=guide_legend(title="Average Goodreads rating"))
p2

library(patchwork)

booktheme <- hrbrthemes::theme_ft_rc(base_size = 8) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        legend.position = 'bottom',
        strip.text.x = element_text(size = 10))

png(filename = "19_NYTBestsellers.png",width = 12,height = 8,units = 'in',res = 200)
p1 + p2 + plot_annotation(title = "How does Goodreads treat the NYT bestsellers?",
                          subtitle = "I've always been curious about how representative the Goodreads community is of broader readership.",
                          caption = "Data: goodreadsbooks from Kaggle and Post45 Data") & booktheme
dev.off()


# Bonus investigation: more correlations! ---------------------------------

baseplot2 <- dat_matches %>%
  ggplot(aes(x=total_weeks,y=ratings_count)) + geom_point()

manyreviews <- filter(dat_matches,ratings_count>700000) %>% mutate(title = stringr::str_to_title(title))

p3 <- baseplot2 + 
  geom_point(data=manyreviews, 
                      aes(x=total_weeks, y=ratings_count)) +
  ggrepel::geom_text_repel(data=manyreviews, 
                           aes(x=total_weeks,y=ratings_count, 
                               label = title), size = 3
  ) +
  xlab("Weeks on NYT bestsellers list") +
  ylab("Number of ratings on Goodreads")
  
png(filename = "NumReviewsVsWeeks.png",width = 6,height = 5,units = 'in',res=200)
p3
dev.off()

gr2 %>% ggplot(aes(x=as.numeric(ratings_count),y=as.numeric(average_rating))) +geom_point()
