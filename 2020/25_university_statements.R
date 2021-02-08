#25_university_statements
library(tidyverse)
library(snakecase)
library(hrbrthemes)
library(ggtext)
library(patchwork)

dat <- read.csv('statements.csv')
head(dat)

# Clean up the data a little
colnames(dat) <- to_any_case(colnames(dat),"snake")
colnames(dat) <- gsub("x_","",colnames(dat))

dat <- dat %>%
  select(-conference,
         -strength_of_statement_mentioning_good_cops_1_7,
         -follow_up_link,
         -statement_link) %>%
  mutate(school_name = recode(school_name,"Louisiana State University and Agricultural & Mechanical College" = "Louisiana State University \n and Agricultural & Mechanical College"))

props2plot <- dat %>% 
  select(-statement_text) %>%
  pivot_longer(cols=on_twitter:good_cops) %>%
  select(-strength_of_statement,-follow_up_updated_statement) %>%
  mutate(val_bin = ifelse(value=="Yes",1,0)) %>%
  group_by(name) %>%
  summarize(prop.mentioned = sum(val_bin)/length(val_bin)) %>%
  ungroup() %>%
  mutate(labelnames = to_title_case(name)) %>%
  mutate(labelnames = recode(labelnames,"Murder Killing" = "Murder/Killing",
                             "Mentions Blackness Discrimination Against Black People" = "Mentions Blackness \n or discrimination against Black people")) %>%
  filter(name!="on_twitter") %>%
  filter(name!="good_cops")


p1 <- props2plot %>%
  ggplot(aes(x=fct_reorder(labelnames,prop.mentioned),y=prop.mentioned)) +
  geom_col(fill='white') +
  xlab("Name or Phrase") +
  ylab("Proportion of school statements mentioning name or phrase") +
  coord_flip() +
  hrbrthemes::theme_ft_rc()+
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)))


stdat <- dat %>%
  group_by(school_name) %>%
  summarize(statement_length = stringi::stri_count_words(statement_text)) %>%
  left_join(dat)

p2 <- stdat %>%
  filter(on_twitter %in% c("Yes","No")) %>%
  filter(statement_length>5) %>%
  ggplot(aes(x = statement_length, y = strength_of_statement,
             colour = on_twitter)) +
  geom_point(size = 2.3,alpha = 0.5) +
  scale_colour_manual("Statement on Twitter?",
                      values=c('white','#9ecae1')) +
  xlab("Length of statement (words)") +
  ylab("Score of statement") +
  labs(caption = "Statement score is the number of \n key names/phrases in the university's primary statement. Dataset: @amaan_c") +
  theme_ft_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)))


vp <- dat %>% 
  select(-statement_text) %>%
  pivot_longer(cols=on_twitter:good_cops) %>%
  select(-strength_of_statement,-follow_up_updated_statement) %>%
  mutate(val_bin = ifelse(value=="Yes",1,0)) %>%
  filter(name != 'good_cops') %>%
  filter(name != 'on_twitter') %>%
  group_by(school_name) %>%
  summarize(score = sum(val_bin)) %>%
  arrange(desc(score)) %>%
  group_by(score) %>%
  mutate(count1 = 1,cc = cumsum(count1))

mentions_good_cops <- filter(dat, good_cops == 'Yes') %>% 
  select(school_name) %>% 
  as.vector()

vp2 <- filter(vp,school_name %in% mentions_good_cops$school_name)

p3 <- vp %>% ggplot(aes(x=score,y=cc)) + 
  annotate("text",x = vp$score, y = vp$cc,
           label = vp$school_name, size = 3, 
           color = "lightgrey",
           family = "Arial Narrow") +
  annotate("text",x = vp2$score, y = vp2$cc,
           label = vp2$school_name, size = 3, 
           color = "red",
           family = "Arial Narrow") +
  hrbrthemes::theme_ft_rc() +
  xlab("Score (Total name and phrase mentions)") +
  ylab("") +
  theme(panel.grid.major=element_blank(),
        panel.grid.minor = element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(title = "How thorough were university statements since George Floyd's murder?",
       subtitle = "<span style = 'color:#ff0000'>Red</span> universities also mentioned 'good cops' in their statement.",
       caption = " Data source: Public university statements, collected and scored by @amaan_c") +
  theme(plot.subtitle = element_markdown(lineheight = 1.1),
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5)))

png("First.png",width = 14,height = 10,units = 'in',res = 120)
p3
dev.off()

png("Second.png",width = 10,height = 12,units = 'in',res=120)
p1 + p2 + plot_layout(ncol = 1)
dev.off()
# Extra bits --------------------------------------------------------------
# p2 <- dat %>% 
#       select(-statement_text) %>%
#       pivot_longer(cols=on_twitter:good_cops) %>%
#       select(-strength_of_statement,-follow_up_updated_statement) %>%
#       mutate(val_bin = ifelse(value=="Yes",1,0)) %>%
#       filter(name != 'good_cops') %>%
#       filter(name != 'on_twitter') %>%
#       group_by(school_name) %>%
#       summarize(score = sum(val_bin)) %>%
#       ggplot(aes(x=fct_reorder(school_name,score),y=score)) +
#       geom_col(fill='white') +
#       xlab("School name") +
#       ylab("Score (sum of mentions of each name/phrase") +
#       coord_flip() +
#       hrbrthemes::theme_ft_rc()
