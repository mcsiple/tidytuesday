#Beach volleyball
library(tidyverse)
library(ggchicklet)
library(hrbrthemes)
library(ggthemes)
library(patchwork)

vb_matches <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-19/vb_matches.csv', guess_max = 76000)

unique(vb_matches$country)
unique(vb_matches$w_p1_country)
unique(vb_matches$circuit)


pd <- vb_matches %>%
  select(circuit,
         tournament,
         country,
         year,
         w_p1_country,
         w_p1_tot_attacks,
         w_p1_tot_kills,
         w_p1_tot_aces,
         w_p1_tot_blocks,
         w_p1_tot_digs) %>%
  pivot_longer(cols = w_p1_tot_attacks:w_p1_tot_digs,
               names_to = c('winner','player','tot','move'),
               names_sep="_",
               values_to = 'n') %>%
  group_by(circuit,tournament,country,year,w_p1_country,winner,tot,move) %>%
  summarize(N = sum(n)) %>%
  ungroup() %>%
  select(-winner,-tot)

p1 <- pd %>% 
      filter(circuit=='FIVB' & !is.na(N)) %>%
      mutate(w_p1_country = fct_drop(w_p1_country)) %>%
      group_by(w_p1_country,move) %>%
      summarize(sum_moves = sum(N,na.rm=T)) %>%
      ggplot(aes(x=fct_reorder(w_p1_country,.x = sum_moves,.fun = sum),y=sum_moves,fill=move)) +
      xlab("Winning country (FIVB)") +
      ylab("Moves") +
      geom_chicklet() +
      coord_flip() +
      scale_fill_tableau("Miller Stone", name = NULL) +
      theme_ipsum_rc(grid="X") +
      theme(axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            plot.margin = margin(1,1,1,1)) + 
  labs(title = 'Moves by winning teams',
       subtitle = 'Total moves in all winning matches',
       caption = 'Data: Adam Vagnar')

p1


# Now maybe we can look at wins over time for each country
pd2 <- vb_matches %>%
        select(year,circuit,tournament,country,match_num,w_p1_country) %>%
        filter(circuit =='FIVB') %>%
        group_by(w_p1_country,year) %>%
        count()




cpal <- lengthen_pal(x=1:9,shortpal = tableau_color_pal(palette = "Summer")(8) )

pd2a <- pd2 %>% 
        group_by(w_p1_country) %>%
        filter(sum(n)>2000) %>% # "big shot" countries have more than 2000 total wins
        ungroup() %>%
        mutate(bigshot = 1) %>%
        full_join(pd2) %>%
        replace_na(list(bigshot = 0))
  
p2 <- pd2a %>%
      mutate(w_p1_country = fct_drop(w_p1_country)) %>%
      ggplot(aes(x=year,y=n,colour=w_p1_country)) +
      geom_line(lwd=1) +
      scale_colour_manual('Country',
                          values = cpal) +
      scale_x_continuous(limits = c(2001,2019)) +
      xlab('Year') +
      ylab('Total matches won') +
      theme_ipsum_rc(grid="X") +
      theme(axis.title.x = element_text(size = rel(1.5)),
            axis.title.y = element_text(size = rel(1.5)),
            legend.position = 'bottom',
            plot.margin = margin(1,1,1,1)) +
      gghighlight::gghighlight(bigshot==1,use_direct_label = FALSE) +
  facet_wrap(~w_p1_country) + 
  labs(title = 'Wins over time',
       subtitle = 'For the nine "big shot" countries with over 2000 total wins',
       caption = 'Data: Adam Vagnar')

  
png(filename = 'Volleyball1.png',width = 10,height = 6,units = 'in',res = 200)
p1 
dev.off()

png(filename = 'Volleyball2.png',width = 10,height = 6,units = 'in',res = 200)
p2
dev.off()


