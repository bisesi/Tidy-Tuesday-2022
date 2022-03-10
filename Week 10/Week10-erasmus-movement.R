#ATB
#Tidy Tuesday W10 2022
#Erasmus 

#load packages
library("tidyverse")
library("circlize")
library("ggplot2")
library("cowplot")
library("showtext")
library("ggtext")
library("countrycode")
library("viridis")
library("ggplotify")

#load data
erasmus <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-08/erasmus.csv')

#sending and receiving
top5sending <- erasmus %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(sending_country_code) %>%
  summarize(n = sum(participants))%>%
  arrange(desc(n)) %>%
  head(5) %>%
  mutate(going_from = countrycode(sending_country_code, origin = "iso2c", destination = "iso.name.en")) %>%
  mutate(going_from = case_when(sending_country_code == "EL" ~ "Greece",
                              sending_country_code == "UK" ~ "United Kingdom",
                              sending_country_code == "NL" ~ "Netherlands",
                              TRUE ~ going_from))

top5receiving <- erasmus %>%
  filter(sending_country_code != receiving_country_code) %>%
  group_by(receiving_country_code) %>%
  summarize(n = sum(participants))%>%
  arrange(desc(n)) %>%
  head(5) %>%
  mutate(going_to = countrycode(receiving_country_code, origin = "iso2c", destination = "iso.name.en")) %>%
  mutate(going_to = case_when(receiving_country_code == "EL" ~ "Greece",
                                receiving_country_code == "UK" ~ "United Kingdom",
                                receiving_country_code == "NL" ~ "Netherlands",
                                TRUE ~ going_to))
  
chords <- erasmus %>%
  select(sending_city, sending_country_code, receiving_city, receiving_country_code, participants) %>%
  mutate(going_to = countrycode(receiving_country_code, origin = "iso2c", destination = "iso.name.en"),
         going_from = countrycode(sending_country_code, origin = "iso2c", destination = "iso.name.en")) %>%
  mutate(going_to = case_when(receiving_country_code == "EL" ~ "Greece",
                              receiving_country_code == "UK" ~ "United Kingdom",
                              receiving_country_code == "XK" ~ "Kosovo",
                              receiving_country_code == "NL" ~ "Netherlands",
                              TRUE ~ going_to)) %>%
  mutate(going_from = case_when(sending_country_code == "EL" ~ "Greece",
                              sending_country_code == "UK" ~ "United Kingdom",
                              sending_country_code == "XK" ~ "Kosovo",
                              sending_country_code == "NL" ~ "Netherlands",
                              TRUE ~ going_from)) %>%
  filter(going_to != going_from) %>%
  filter(going_to %in% top5receiving$going_to & going_from %in% top5sending$going_from) %>%
  group_by(going_to, going_from) %>%
  summarize(n = sum(participants)) %>%
  arrange(desc(n))

#formatting
font_add_google("Jetbrains Mono")
showtext_auto()
colors <- viridis(7)

#plot
chordDiagram(chords, grid.col = colors)

labels <- ggplot()+
  labs(title = "Erasmus Student Movement",
       subtitle = "Movement of participants between most popular countries from 2014 to 2020",
       caption = "Data: Data.Europa \nVisualization: A Bisesi") +
  theme(text = element_text("Jetbrains Mono"),
        panel.background = element_rect(fill='transparent'),
        plot.background = element_rect(fill='transparent', color=NA),
        plot.title = element_text(hjust = 0.5, size = 24, face = "bold"),
        plot.subtitle = element_text(size = 16), 
        plot.caption = element_text(size = 8))

ggdraw(labels)+
  draw_image("/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 10/chord_diagram.png",
             x = 0.08, y = 0.001, height = 0.90, width = 0.90)



