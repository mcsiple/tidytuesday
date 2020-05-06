# ANIMAL CROSSING!!!

library(tidyverse)
library(rfishbase)
library(calecopal)
library(patchwork)

# coding soundtrack: Edith Piaf 

items <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-05/items.csv')
load("PolygonData.RData") # load data from Polygon site - tibble pdat includes Location and Shadow.Size

getspinfo <- function(commonname){
  nm <- common_to_sci(paste(commonname))
  dat <- species(nm$Species, fields=c("Species", "PriceCateg", "Vulnerability"))
  nspecies <- nrow(dat)
  pricecat <- sort(table(dat$PriceCateg),decreasing=TRUE)
  x <- names(pricecat)[1]
  modeprice <- ifelse(x!='unknown',x,names(pricecat[2]))
  meanvuln <- mean(dat$Vulnerability,na.rm = T)
  return(list(n = nspecies,price_mode = modeprice,vuln = meanvuln))
}

# Let's be honest...
fishes <- items %>% 
            filter(category  =='Fish')

RLprice = RLvuln = nspps = vector()

# Didn't work with mutate() and I don't have time to figure out why
for(i in 1:nrow(fishes)){
  x <- getspinfo(fishes$name[i])
  if(length(x$price_mode)==0){RLprice[i] = RLvuln[i] = nspps[i] = NA}else{
  RLprice[i] <- x$price_mode
  RLvuln[i] <- x$vuln
  nspps[i] <- x$n
  }
}

fishvuln <- fishes %>%
  add_column(RLprice = RLprice) %>%
  add_column(RLvuln = RLvuln) %>%
  add_column(nspps = nspps)

p1 <- fishvuln %>%
  ggplot(aes(x=RLvuln,y=sell_value,colour=RLprice)) +
  geom_point() +
  scale_color_manual('Price in real life',values = cal_palette("superbloom1")) +
  xlab('Mean vulnerability score in real life') +
  ylab('Sell value in Animal Crossing (bells)') +
  hrbrthemes::theme_ft_rc()

#Labels for extreme values
extremes1 <- fishvuln %>% 
  filter(nspps>500) %>%
  distinct(name,.keep_all=TRUE)
extremes2 <-  fishvuln %>% 
  filter(sell_value>12500) %>%
  distinct(name,.keep_all=TRUE)

p2 <- fishvuln %>%
  ggplot(aes(x=sell_value,y=nspps)) +
  geom_point(colour='white') +
  xlim(c(0,20000)) +
  ggrepel::geom_text_repel(data=extremes1,
                           aes(x=sell_value,y=nspps,label=name),
                           colour='white') +
  ggrepel::geom_text_repel(data=extremes2,
                           aes(x=sell_value,y=nspps,label=name),
                           colour='white',
                           nudge_x = 5000,
                           nudge_y = 500,
                           vjust=0,
                           direction = 'y') +
  #scale_color_manual('Price in real life',values = cal_palette("superbloom1")) +
  xlab('Sell value in Animal Crossing (bells)') +
  ylab('Number of species in real life with this common name') +
  hrbrthemes::theme_ft_rc()
scatplot2


habs <- fishvuln %>%
  left_join(pdat,by = c("name"="Fish")) %>%
  select(name,Location,sell_value) %>%
  distinct(name,.keep_all=TRUE)

p3 <- habs %>%
  group_by(Location) %>%
  filter(length(name)>2) %>%
  ggplot(aes(x=sell_value,fill=Location)) +
  geom_density(colour='white') +
  scale_fill_manual(values = cal_palette("chaparral2")) +
  facet_wrap(~Location,nrow=1) +
  xlab('Sell value (bells)') +
  ylab("Density") +
  coord_flip() +
  hrbrthemes::theme_ft_rc() +
  theme(axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position = 'none') +
  labs(title = 'How valuable are fish in Animal Crossing',
       subtitle = "...and how does their value compare to the same taxa in real life?",
       caption = "Data: VillagerDB, FishBase, and Polygon")
  
png(filename = 'AnimalCrossing.png',width = 12,height = 8,units = 'in',res = 120)
p3 / (p1+p2 + plot_layout(widths=c(1,2))) 
dev.off()
