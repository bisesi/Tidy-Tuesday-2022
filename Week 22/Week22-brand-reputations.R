#ATB
#Tidy Tuesday W22 2022
#Top brands

#load packages
library("tidyverse")
library("ggdist")
library("showtext")
library("ggtext")

#import data
poll <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv")
reputation <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv")

# highest and lowest scores in all categories
notablescores <- reputation %>%
  group_by(name) %>%
  slice(which(score == max(score)), which(score == min(score))) %>%
  ungroup()

#get labels to reorder
label_position <-reputation %>%
  group_by(name) %>%
  summarise(lab_position = min(score)+5) %>%
  ungroup()

#import fonts
sysfonts::font_add_google("Anton","Anton")
font_add_google("Anton","Anton")
font_add_google("Bebas Neue","Bebas Neue")
showtext_auto()

# main plot
plot <- reputation %>%
  ggplot() +
  geom_text(aes(x =reorder(name, - lab_position), y = lab_position, label = name),
    data = label_position,
    family = "Anton",
    color = "#ECB390",
    fontface = "bold", vjust = 0, hjust = 0.01,
    size = 8) +
  stat_dots(aes(x =factor(name), y = score),
            color = "#DD4A48",
            fill = "#DD4A48",
            side = "right", dotsize = 0.8) +
  stat_pointinterval(aes(x =name, y = score),
                     side = "left", position=position_nudge(x = -.03, y = 0)) +
  coord_flip() +
  theme_void() +
  labs(title = "TOP 100 BRANDS IN AMERICA",
    subtitle = "Distribution of the top 100 US brands across the seven key dimensions of reputation",
    caption = "Visualization: A Bisesi | Data: Axios-Harris Poll") +
  theme(legend.position = "none",
        plot.title = element_text( face = "bold",size = 25,family = "Anton", color ="#DD4A48", hjust = 0.5), 
        plot.subtitle = element_text( face = "bold",size = 15, color ="#ECB390",family = "Anton", hjust = 0.5),
        plot.caption = element_text( face = "bold",size = 10, color ="#ECB390",family = "Anton", hjust = 0.5),
        plot.margin =unit(c(1,1,1,1), "cm" ))
