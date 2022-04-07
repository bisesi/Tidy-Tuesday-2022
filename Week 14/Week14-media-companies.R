#ATB
#Tidy Tuesday W14 2022
#News publications

#load packages
library("tidyverse")
library("showtext")
library("waffle")
library("MetBrewer")

#load data
data <- tidytuesdayR::tt_load('2022-04-05')
news_orgs <- data$news_orgs

#tax data
taxes <- news_orgs %>% 
  select(tax_status_current) %>% 
  drop_na() %>%
  group_by(tax_status_current) %>%
  count() 

taxes$total <- sum(taxes$n)

statuses <- c("For Profit", "LLC", "Nonprofit 501c3", "Not for Profit", "Partnership",
              "Public Benefit Corporation", "S Corp", "Sole Proprietor", "501c3 Umbrella")

taxes <- setNames(taxes$n, statuses)

#plot
showtext_auto()
sysfonts::font_add_google("Merriweather", "Merriweather")

plot <- waffle(taxes, rows = 18) + 
  theme_void() +
  scale_fill_manual(values = met.brewer("Klimt", 9)) +
  labs(title = "Media Company Tax Statuses",
       subtitle = "Tax Statuses of North American Media Companies",
       caption = "Data: Project Oasis | Visualization: A Bisesi") +
  theme(text = element_text(family = "Merriweather"),
    legend.title = element_blank(),
    plot.background = element_rect(fill = "#efedf5", color = NA),
    plot.title = element_text(margin = margin(15, 0, 0, 0), size = 30, family = "Merriweather"),
    plot.subtitle = element_text(margin = margin(10, 0, 10, 0), size = 14),
    legend.text = element_text(size = 12, family = "Merriweather"),
    plot.caption = element_text(hjust = 0, margin = margin(10, 0, 10, 0)),
    plot.margin = margin(1, 1, 1, 1, unit = "cm"),
    legend.key.size = unit(0.1, "cm"),
    legend.key.width = unit(0.1, "cm"))

