#ATB
#Tidy Tuesday W6 2022
#Tuskegee Airmen

#load packages
library("ggplot2")
library("tidyverse")
library("showtext")
library("ggtext")

#load dataset
airmen <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-08/airmen.csv')

#get hometown information
hometowns <- airmen %>%
  count(military_hometown_of_record) %>%
  arrange(desc(n)) %>%
  filter(military_hometown_of_record == "Chicago" |
         military_hometown_of_record == "Detroit" |
         military_hometown_of_record == "Cleveland" |
         military_hometown_of_record == "Minneapolis" |
         military_hometown_of_record == "Indianapolis")

#Set number for a complete revolution of the spiral (~0.5*max(n))
revolution <- 38

# Create data frame with x and y values for use in the spiral graph
#rev is the number where that spiral ends, x is the length of the spiral and then must be motified for the length of the line in final revolution
#2 is default y value, drop y value by 1 per full revolution
#Create buffer ID to prevent overlap, divide by number of values (5) and modify until the plot looks satisfactory
spiral <- hometowns %>% 
  group_by_all() %>% 
  summarize(rev = 1:ceiling(n / revolution)) %>%  
  group_by_all() %>% 
  summarize(x = c(0, n)) %>%
  ungroup() %>% 
  mutate(x = case_when(
      x == 0 ~ 0,
      rev == 1 & x < revolution ~ x,
      rev == 1 & x > revolution ~ revolution,
      rev == 2 & x > revolution ~ x - revolution),                
    y = case_when(
      rev == 1 & x == 0 ~ 2,
      rev == 1 & x != 0 ~ 2 - x / revolution,
      rev == 2 & x == 0 ~ 1,
      rev == 2 & x != 0 ~ 1 - x / revolution)) %>%
  arrange(n)%>%
  group_by(military_hometown_of_record) %>% 
  mutate(buffer = case_when(military_hometown_of_record == "Minneapolis" ~ 1,
                            military_hometown_of_record == "Indianapolis" ~ 2,
                            military_hometown_of_record == "Cleveland" ~ 3,
                            military_hometown_of_record == "Detroit" ~ 4,
                            military_hometown_of_record == "Chicago" ~5)) %>%
  mutate(y = y + (0.5-buffer)/5)

#Generate abels for graph 
textlabels <- arrange(spiral, n) %>% 
  mutate(military_hometown_of_record = case_when(military_hometown_of_record == "Minneapolis" ~ "MINNEAPOLIS",
                                                 military_hometown_of_record == "Indianapolis" ~ "INDIANAPOLIS",
                                                 military_hometown_of_record == "Cleveland" ~ "CLEVELAND",
                                                 military_hometown_of_record == "Detroit" ~ "DETROIT",
                                                 military_hometown_of_record == "Chicago" ~ "CHICAGO")) %>%
  filter(x == 0, rev == 1) %>% 
  mutate(text = paste0(military_hometown_of_record, " - ", n))

#DuBois stylization from style guide
tan <- "#d2b48c"
brown <- "#654321"
gold <- "#ffd700"
red <- "#dc143c"
pink <- "#ffc0cb"
green <- "#00aa00"
blue <- "#4682b4"
font_add_google("JetBrains Mono", family = "jetbrains")
font_add_google("B612 Mono", family = "b612")
showtext_auto()

#Make plot, spiral goes pink > blue > brown > gold > grey > red
Mainplot <- ggplot() + 
  geom_line(data = spiral, aes(x = x, y = y, group = interaction(n, rev),
                                    color = factor(military_hometown_of_record)), size = 4) + 
  geom_text(data = textlabels, aes(x = revolution - 0.5, y = y, label = text),
            hjust = 1, size = 3, family = "b612") + 
  labs(title = "TUSKEGEE AIRMEN",
    subtitle = "MAJOR MIDWESTERN HOMETOWNS OF RECORD",
    caption = "DATA: COMMEMORATIVE AIR FORCE | VISUALIZATION: A BISESI") + 
  coord_polar() + 
  scale_y_continuous(expand = c(0,0), limits = c(-3, 2.61)) +
  scale_color_manual(values = c(red, gold, brown, blue, pink)) + 
  guides(color = "none") +
  theme_void() + 
  theme(plot.margin = margin(5, 0, 5, 0),
    plot.background = element_rect(color = tan, fill = tan),
    plot.title = element_text(family = "jetbrains", size = 20, hjust = 0.5),
    plot.subtitle=element_text(family = "jetbrains", size = 17, hjust = 0.5),
    plot.caption = element_text(family = "jetbrains", size = 8))

