library(tidyverse)


tuesdata <- tidytuesdayR::tt_load(2021, week = 5)
plastics <- tuesdata$plastics
totplastics <- plastics %>%
  group_by(parent_company, year) %>%
  summarize_at(vars(empty:pvc), .funs = ~ sum(.x, na.rm = TRUE))


top7 <- totplastics %>%
  filter(year == 2020) %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  group_by(parent_company) %>%
  summarize(total = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(parent_company = case_when(
    parent_company %in% c("Unbranded", "null", "NULL", "#ERROR!") ~ "Unbranded_unknown",
    TRUE ~ parent_company
  )) %>%
  mutate(company_lumped = fct_lump(parent_company, n = 7, w = total)) %>%
  distinct(parent_company, company_lumped) %>%
  right_join(totplastics) %>%
  mutate(total = hdpe + ldpe + o + pet + pp + ps + pvc) # forgot how to sum across columns


companypal <- c("#81C4CA",
                "#468D96",
                "#103128",
                "#E83D5F",
                "#FA6E90",
                "#FCB16D")

p1 <- top7 %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  mutate(type = fct_reorder(type, desc(value))) %>%
  filter(company_lumped != "Unbranded_unknown") %>%
  filter(company_lumped != "Other") %>%
  filter(!type %in% c("ps", "pvc", "empty")) %>%
  group_by(company_lumped, type) %>%
  summarize(totvalue = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(company_lumped) %>%
  mutate(prop_val = totvalue / sum(totvalue)) %>%
  ggplot(aes(fill = company_lumped, colour = company_lumped)) +
  geom_segment(aes(
    x = type, xend = type,
    y = 0.02, yend = prop_val + 0.02
  ),
  lwd = 2, lineend = "round"
  ) +
  geom_label(aes(x = type, y = 0, label = type),
    fill = "white", size = 2.5
  ) +
  ylim(c(0, 1.2)) +
  xlab("Type of plastic") +
  ylab("") +
  scale_colour_manual(values = companypal) +
  coord_polar(theta = "y") +
  facet_wrap(~company_lumped, ncol = 2) +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    legend.position = "none"
  ) +
  labs(caption = "Data: Break Free From Plastic")

p1

# Heat map! ---------------------------------------------------------------
top30 <- totplastics %>%
  filter(year == 2020) %>%
  pivot_longer(cols = empty:pvc, names_to = "type") %>%
  group_by(parent_company) %>%
  summarize(total = sum(value, na.rm = T)) %>%
  ungroup() %>%
  mutate(parent_company = case_when(
    parent_company %in% c("Unbranded", "null", "NULL", "#ERROR!") ~ "Unbranded_unknown",
    TRUE ~ parent_company
  )) %>%
  mutate(company_lumped = fct_lump(parent_company, n = 30, w = total)) %>%
  distinct(parent_company, company_lumped) %>%
  right_join(totplastics) %>%
  mutate(total = hdpe + ldpe + o + pet + pp + ps + pvc) # forgot how to sum across columns

p2 <- top30 %>%
  group_by(company_lumped) %>%
  summarize_at(vars(empty:pvc), .funs = ~ sum(.x, na.rm = TRUE)) %>%
  filter(!is.na(company_lumped)) %>%
  mutate(row_sum = rowSums(select(., -1))) %>%
  mutate_at(-1, ~ . / row_sum) %>%
  select(-row_sum) %>%
  pivot_longer(cols = empty:pvc) %>%
  ggplot(aes(x = name, y = company_lumped, colour = value)) +
  geom_point(size = 4.5) +
  scale_colour_distiller(palette = "YlGnBu") +
  scale_y_discrete(limits = rev) +
  xlab("Type of plastic") +
  ylab("") +
  hrbrthemes::theme_ipsum_rc() +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5)
  ) +
  labs(
    title = "Which type of plastic waste do these top companies produce?",
    subtitle = "Based on labeled products from an audit report by Break Free From Plastic"
  )

p2

library(patchwork)
png("5_2021_plastics.png", width = 14, height = 10, units = "in", res = 200)
p2 + p1 + plot_layout(ncol = 2, widths = c(2, 3))
dev.off()
