#37_Friends

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2020-09-08')

friends <- tuesdata$friends
friends_info <- tuesdata$friends_info
friends_emotions <- tuesdata$friends_emotions

