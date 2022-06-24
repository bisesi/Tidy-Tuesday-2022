#ATB
#TidyTuesday W25 2022
#Juneteenth

#load packages
library("showtext")
library("tidyverse")
library("ggstream")

#load data
firsts <- read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-21/firsts.csv")

#clean data for plot
accomplishments <- firsts %>%
  filter(gender == "African-American Firsts") %>%
  group_by(year, category) %>%
  tally()

#use ggstream for plot
plot <- accomplishments %>%
  ggplot(aes(x = year,y = n, group = category,fill = category)) +
  geom_stream() +
  scale_fill_brewer(palette = "Paired") +
  guides(fill = guide_legend(ncol = 2)) +
  labs(x = "",y = "",fill = "",
    title = "African-American Firsts",
    caption = "Visualization: A Bisesi") +
  theme_minimal() +
  theme(axis.line = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length.x = unit(.5, "cm"),
    axis.text.x = element_text(size = 14),
    legend.position = c(.25, .8),
    legend.text = element_text(size = 8),
    panel.grid.major.x = element_line(color = "grey80", linetype = "dashed"),
    plot.title = element_text(size = 24, face = "bold"),
    plot.subtitle = element_text(color = "grey50", size = 18),
    plot.caption = element_text(size = 8))