#ATB
#Tidy Tuesday W23 2022
#Corporate anti-queer donations

#load packages
library("tidyverse")
library("treemapify")
library("RColorBrewer")

#import data
pride_aggregates <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-07/pride_aggregates.csv") %>%
  filter(Company != "Grand Total")

#combine color palettes for a total of 31 colors
mycolors <- unique(c(brewer.pal(name="Dark2", n = 8), brewer.pal(name="Paired", n = 12), brewer.pal(name = "Spectral", n = 10))) %>%
  cbind(Company = pride_aggregates$Company, color = .) %>%
  as_tibble() 

#plot
font_add_google("Source Sans Pro", "pro")
showtext_auto()

plot <- pride_aggregates %>%
  left_join(mycolors) %>%
  mutate(Company = str_wrap(Company, 14)) %>%
  rename(Contributed = `Total Contributed`) %>%
  ggplot(aes(fill = color, color = after_scale(prismatic::best_contrast(fill, y = c("#111111", "#FFFFFF"))), area = Contributed, label = scales::comma(round(Contributed)), subgroup = Company)) +
  geom_treemap() +
  geom_treemap_subgroup_border(colour = "#111111", size = 3) +
  geom_treemap_subgroup_text(place = "topleft", grow = T, min.size = 0) +
  geom_treemap_text(place = "bottomright",alpha = .5, grow = T, reflow = T) +
  labs(title = "Contributions from major companies to anti-LGBTQ campaigns",
    subtitle = "Many companies are publicly declaring support for LGBTQ people while quietly donating money to anti-LGBTQ politicians",
    caption = "Visualization: A Bisesi | Data: Data for Progress") +
  scale_fill_identity() +
  theme_void() +
  theme(plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(c(1, 1, 1, 1), unit = "cm"),
    plot.title = element_text(hjust = .5, family= "pro", face = "bold", size = 24),
    plot.subtitle = element_text(hjust = .5, family = "pro", size = 16),
    plot.caption = element_text(hjust = .5, family = "pro"))
