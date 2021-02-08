# Beer
library(tidyverse)
library(lubridate)
library(patchwork)

brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')

dg = "#222222"
pal <- calecopal::cal_palette(name = "chaparral2", n = 6)[c(5,6)]

# Density plot ------------------------------------------------------------

s2 <- brewing_materials %>% 
  group_by(year, type) %>%
  filter(material_type %in% c('Grain Products',"Non-Grain Products"))

densplot <- s2 %>%
  rename("Material type"=material_type) %>%
  ggplot(aes(month_current/1e6,
             colour=`Material type`,
             fill=`Material type`)) +
  geom_density(lwd=1) +
  scale_colour_manual(values = pal) +
  scale_fill_manual(values = colorspace::lighten(pal,amount=0.5))+
  hrbrthemes::theme_ft_rc(base_size=14) +
  theme(strip.background = element_blank(),
        strip.text = element_blank()) +
  ylab('Density') +
  xlab(bquote('Material used (millions of pounds)')) +
  facet_wrap(~`Material type`,ncol=1,scales='free_y') +
  theme(legend.position = 'bottom') +
  labs(title = 'Materials used for beer production',
       subtitle = '2008-2017')

# Time series -------------------------------------------------------------

s <- brewing_materials %>%
  group_by(year, type) %>%
  summarize(totprod = sum(month_current,na.rm=T))

matchtable <- brewing_materials %>%
  distinct(material_type,type) 

s <- s %>%
  ungroup() %>%
  left_join(matchtable) %>%
  filter(material_type!='Total Used') %>%
  filter(type != 'Total Grain products') %>%
  filter(type != 'Total Non-Grain products')

labeldata <- s %>% 
  filter(year == min(year))

timeplot <-  s %>%
  ggplot(aes(x=year,y=totprod/1e6,
             colour=material_type,
             group=type)) +
  geom_line(lwd=1) +
  scale_colour_manual(values = pal) +
  scale_x_continuous(breaks = unique(s$year)) +
  hrbrthemes::theme_ft_rc(base_size=14) +
  ggrepel::geom_label_repel(data = labeldata, 
                            aes(label = type),
                            point.padding = 1,
                            fill=dg) +
  ylab('Material used (millions of pounds)') +
  xlab('Year') +
  theme(legend.position = 'none') +
  labs(caption = 'Data: Alcohol and Tobacco Tax and Trade Bureau')
  

png('Beer.png',width = 8,height = 10,units = 'in',res = 120)
densplot/timeplot +
  plot_layout(heights = c(1,2))
dev.off()