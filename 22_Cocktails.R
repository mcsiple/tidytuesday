# Cocktails!

#devtools::install_github("gadenbuie/ggpomological")

library(tidyverse)
library(tidyr)
library(stringr)
library(vegan)
library(patchwork)
library(ggpomological)

# Look at cocktails with fruits in them, for fun!
cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/cocktails.csv')
boston_cocktails <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-05-26/boston_cocktails.csv')

test <- c("Cats","Banana","banana")

fruit_list <- tolower(c("Kiwi", "Banana", "Apricot", "Avocado", "Cocos", "Clementine", "Mandarine", "Orange","Cranberry","Cherry","BLackcurrant","Lime", "Lemon", "Peach", "Plum", "Raspberry", "Strawberry", "Pineapple", "Pomegranate","Limon","Apple"))

fruitycocktails <- cocktails %>%
  mutate(ingredient = tolower(ingredient)) %>%
  filter(ingredient %in% fruit_list) %>%
  select(drink) %>%
  unique()

# Filter out fruity drinks
fdrinks <- cocktails %>%
  group_by(drink) %>%
  right_join(fruitycocktails) 

# Tried to sort out amounts, too complicated for 2 hrs of coding... I fail!

p1 <- fdrinks %>% 
  mutate(alcoholic = tolower(alcoholic)) %>%
  distinct(drink,category,alcoholic) %>% #subset to distinct drinks
  group_by(category,alcoholic) %>%
  count() %>%
  ggplot(aes(x=fct_reorder(category,desc(-n)),y=n,fill=alcoholic))+
  geom_col() +
  xlab('Category') +
  ylab('Number of drinks') +
  coord_flip() +
  scale_fill_pomological('') +
  labs(caption = 'Data: Kaggle')+
  theme(legend.position = 'bottom') +
  theme_pomological("Homemade Apple", 16) 



# Turn fruity drinks into community matrix --------------------------------
cm <- fdrinks %>%
  pivot_wider(id_cols = drink,
              names_from = ingredient,
              values_from = row_id,
              values_fn = list(row_id = length)) %>%
  mutate_if(is.numeric, funs(replace_na(., 0))) # replace NA with 0

cmat <-as.matrix(sapply(cm[,-1], as.numeric))
subset_ingredients <- which(colSums(cmat)>10)
cmat <- cmat[,subset_ingredients]
subset_drinks <- which(rowSums(cmat) != 0)
cmat <- cmat[subset_drinks,]
drink <- cm$drink[subset_drinks]
mds.log <- log(cmat+1)
sol <- metaMDS(mds.log)
vec.sp <- envfit(sol$points, mds.log, perm=500)
vec.sp.df <- as.data.frame(vec.sp$vectors$arrows*sqrt(vec.sp$vectors$r))
vec.sp.df$species <- rownames(vec.sp.df)
vec.sp.df.tp <- vec.sp.df
ord <- sol$points %>%
  as.data.frame() %>%
  add_column(drink = drink) %>%
  left_join(fdrinks) %>%
  distinct(id_drink,.keep_all = TRUE)

p2 <- ord %>% 
  filter(!is.na(drink)) %>%
  ggplot(aes(MDS1,MDS2,colour=category)) +
  geom_point(size=4) +
  labs(title = 'The community of fruity drinks') +
  scale_colour_pomological("Category") +
  theme_pomological("Homemade Apple", 16)

png('Cocktails.png',width = 10,height = 10,units = 'in',res = 200)
p2 + p1 + plot_layout(ncol = 1, heights=c(3,1)) 
dev.off()
