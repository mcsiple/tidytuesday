# Get the Data
library(tidyverse)
library(lubridate)
library(patchwork)
library(scales)

gdpr_violations <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_violations.tsv')
gdpr_text <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-21/gdpr_text.tsv')

gdpr2 <- gdpr_violations %>% 
  mutate(newdate = mdy(date)) %>%
  filter(newdate > '2000-01-01')


make_numer <- function(x){
  y <- str_extract(x,pattern = "[[:digit:]]+[:space:]") %>%
    str_trim(side = 'both') 
  return(y)
}

# sort out the data and clean up the articles being violated
articles_violated <- gdpr2 %>% 
  select(id,article_violated) %>%
  separate(article_violated,c('n1','n2','n3','n4','n5'),
           sep=paste("\\|"),fill='right') %>%
  mutate_at(c('n1','n2','n3','n4','n5'),make_numer) %>% #extract articles violated!
  pivot_longer(-id) %>%
  drop_na(value) %>%
  select(-name) %>%
  rename('art_viol'=value)

pdat <- articles_violated %>%
  left_join(gdpr2) %>%
  group_by(newdate,art_viol) %>%
  summarize(num_viols = n(),total_price = sum(price)) %>%
  arrange(newdate) %>%
  group_by(art_viol) %>%
  mutate(rolling_numviol = cumsum(num_viols)) %>%
  filter(length(num_viols)>3)

ends <- pdat %>%
  group_by(art_viol) %>%
  top_n(1,newdate) %>%
  mutate(enddate = max(newdate)) %>%
  mutate(artlabel = paste(art_viol))

# set aesthetic stuff
dg = "#222222"
palitra.pal <- c('#96ffff','#52eeff','#17d8ff','#5cbcff','#5cbcff','#a696ff','#e064e6','#ff009f')
nb.cols <- length(levels(factor(pdat$art_viol)))
mycolors <- colorRampPalette(palitra.pal)(nb.cols)


plot1 <- pdat %>% 
  ggplot(aes(x=newdate,y=rolling_numviol,colour=art_viol)) +
  geom_line(lwd=1.2) +
  hrbrthemes::theme_ft_rc() +
  xlab('Date') +
  ylab('Cumulative number of violations') +
  scale_color_manual(values=mycolors) +
  scale_x_date(date_labels = paste0('%b',' 20','%y')) +
  ggrepel::geom_label_repel(data = ends, 
                            aes(x=enddate,
                                y=rolling_numviol,
                                label=artlabel),
                            max.iter = 100,
                            nudge_x = 25,
                            fill = dg) +
  theme(legend.position = 'none',
        axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  labs(title = 'Violations of the General Data Protection Regulation (EU) ',
       subtitle = "Which articles of the GDPR had the most frequent violations?",
       caption = 'Data: GDPR, c/o Bob Rudis')

p2 <- pdat %>% 
  mutate(yr = year(newdate)) %>%
  group_by(yr,art_viol) %>%
  summarize(price = sum(total_price))

totals <- p2 %>% 
  group_by(yr) %>% 
  summarize(totp = sum(price)) %>%
  as.data.frame()

plot2 <- p2 %>% 
  ggplot(aes(x=yr,y=price,fill=art_viol)) + 
  geom_col() +
  scale_fill_manual('Article violated',values=mycolors) +
  xlab('Year') +
  ylab('Total fines ( \u20AC )') +
  hrbrthemes::theme_ft_rc() +
  theme(axis.title.x = element_text(size = rel(1.5)),
        axis.title.y = element_text(size = rel(1.5))) +
  geom_text(data=totals,
            aes(x=yr,y=totp + 5e6,
                label=paste(format(round(totp,-3),big.mark = ','),' \u20AC')),
            inherit.aes = FALSE) +
  scale_y_continuous(labels = comma)

png('GDPR.png',width = 14,height = 8,units = 'in',res = 110)
plot1+plot2 +
  plot_layout(widths = c(2,1))
dev.off()
