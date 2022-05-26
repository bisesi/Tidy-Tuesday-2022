#ATB
#Tidy Tuesday W21 2022
#Rugby

#import packages
library("tidyverse")
library("lubridate")
library("ggtext")
library("showtext")

#load data
sevens <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-24/sevens.csv') %>%
  mutate(year = year(date))

#wins and losses for Scotland
wins <- sevens %>%
  filter(winner == "Scotland") %>%
  group_by(year) %>%
  summarise(outcome = n()) %>%
  mutate(category = "Win") %>%
  ungroup()

losses <- sevens %>%
  filter(loser == "Scotland") %>%
  group_by(year) %>%
  summarise(outcome = n()*-1) %>%
  mutate(category = "Loss") %>%
  ungroup()

winslosses <- rbind(wins, losses) %>%
  filter(year >= 2010) %>%
  group_by(year) %>%
  mutate(games = sum(abs(outcome)),
         percentage = round(abs(outcome)/games*100, 0)) %>%
  ungroup()

#load font
font_add_google("Roboto", "robo")
showtext_auto()

#plot
plot <- winslosses %>%
  ggplot() + 
  geom_bar(aes(x = year,y = outcome,fill = category, width = 0.7), 
           stat = "identity", position = "identity") +
 scale_y_continuous(labels = abs) +
  labs(title = "The Scotland Women's Rugby Sevens Team Wins and Losses",
    x = "", y = "",
    caption = "Visualization: A Bisesi | Data: ScrumQueens") +
  theme_bw() +
  scale_x_continuous(breaks = c(2010, 2011, 2012, 2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)) +
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        plot.title = element_text(face = "bold", family = "robo"),
        axis.text.x = element_text(vjust = 0, face = "bold"))