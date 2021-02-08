library(tidyverse)


tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics
totplastics <- plastics %>% 
  group_by(parent_company, year) %>%
  summarize_at(vars(empty:pvc), .funs = ~ sum(.x, na.rm = TRUE))


top3 <- totplastics %>% 
  filter(year == 2020) %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  group_by(type) %>%
  summarize(total= sum(value, na.rm=T)) %>%
  ungroup() %>%
  slice_max(total, n = 3) 

top4  <- totplastics %>% 
  filter(year == 2020) %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  group_by(parent_company) %>%
  summarize(total = sum(value, na.rm=T)) %>%
  ungroup() %>%
  mutate(parent_company = case_when(
    parent_company %in% c("Unbranded","null","NULL") ~ "Unbranded_unknown", 
    TRUE ~ parent_company)) %>%
  mutate(company_lumped = fct_lump(parent_company, n = 4, w = total)) %>%
  distinct(parent_company, company_lumped) %>%
  right_join(totplastics) %>%
  mutate(total = hdpe + ldpe + o +  pet + pp  +  ps + pvc) # forgot how to sum across columns


top4 %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  mutate(type = fct_reorder(type, desc(value))) %>%
  filter(company_lumped!= "Unbranded_unknown") %>%
  ggplot(aes(x = type, y = value, fill = type, colour = type)) +
  geom_col() +
  #coord_polar(theta = "y") +
  facet_wrap(~company_lumped)
  
