# TBI
library(tidyverse)



# Get data ----------------------------------------------------------------
tbi_age <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_age.csv')
tbi_year <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_year.csv')
tbi_military <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-24/tbi_military.csv')

head(tbi_military)

plotdat <- tbi_military %>%
          filter(component %in%c('Guard','Reserve')) %>%
          group_by(year,service,severity) %>%
          summarize(diag.total = sum(diagnosed))

colpal1 <- RColorBrewer::brewer.pal(n = 6,name = 'RdBu')[-6]
colpal1[3] <- 'lightgrey'

areaplot <- plotdat %>% 
            ggplot(aes(x=year,y=diag.total,fill = severity)) +
            geom_area() +
            facet_wrap(~service,scales='free') +
            scale_fill_manual(values = rev(colpal1)) +
            theme(strip.background = element_blank()) +
            xlab('Year') +
            ylab('Total diagnoses of traumatic brain injury') +
            hrbrthemes::theme_ft_rc() +
  labs(title = 'Brain injuries among guard and reserve \n military service members',
       subtitle = '2006-2014',
       caption = 'Data: CDC')

png('TBI.png',width = 8,height = 8,units = 'in',res = 120)
areaplot
dev.off()
