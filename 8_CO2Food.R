#Feb 18, 2020
# Food consumption and CO2 
library(sf)
library(mapedit)
library(leaflet)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(stringr)

food_consumption <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-02-18/food_consumption.csv')
str(food_consumption)
unique(food_consumption$country)
unique(food_consumption$food_category)

# emissions per pound are very slightly different!
food_consumption %>%
  group_by(food_category) %>%
  mutate(emission_per = co2_emmission/consumption) %>%
  as.data.frame() 

nfc <- food_consumption %>%
  mutate(emiss_per_cons = co2_emmission/consumption)
# But I'm not going to worry about it

# Let's map it!
world <- ne_countries(scale = "medium", returnclass = "sf") 

pe <- food_consumption %>%
  filter(food_category == "Milk - inc. cheese") %>%
  mutate(country.corr = str_replace(country,"USA","United States"))

worldp <- left_join(world,pe,by=c('name'='country.corr'))

dairy <- ggplot(data = worldp) +
          geom_sf(aes(fill=co2_emmission)) +
          scale_fill_viridis_c("Emissions \n (kg/person/yr)",option = 'inferno',direction = -1) +
          theme_minimal() +
          ggtitle(expression('Per capita '~CO[2]~' emissions from milk/cheese consumption'))

png('Dairy.png',width=6,height=4,units = 'in',res=150)
dairy 
dev.off()


# Fancy dark background version -------------------------------------------
# To make the fancy dark ones that everyone makes on twitter
library(extrafont) #not sure which of these is required
library(extrafontdb)
library(hrbrthemes)

dairy2 <- dairy + theme_ft_rc()
dairy2
