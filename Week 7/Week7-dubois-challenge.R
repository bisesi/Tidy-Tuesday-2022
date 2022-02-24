#ATB
#Tidy Tuesday W7 2022
#DuBois Challenge

#load packages
library("ggplot2")
library("tidyverse")
library("showtext")
library("ggtext")
library("cowplot")

#DuBois stylization from style guide
tan <- "#d2b48c"
black <- "#000000"
brown <- "#654321"
gold <- "#ffd700"
red <- "#dc143c"
pink <- "#ffc0cb"
green <- "#008746"
blue <- "#4682b4"
grey <- "grey60"
font_add_google("JetBrains Mono", family = "jetbrains")
font_add_google("B612 Mono", family = "b612")
showtext_auto()

#Create dataset
proportion <- data.frame(years = c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870),
                         free = c(8, 11, 13.5, 13, 14, 13, 12, 11, 100))

proportion <- proportion %>% 
  mutate(slaves = 100 - free)

#Set up plot
ggplot(proportion, aes(x = years)) +
  geom_area(aes(y = 100), fill = green, alpha = 0.8) + 
  geom_area(aes(y = slaves), fill = black) +
  geom_text(aes(y = slaves, label = c("8%", "11%", "13.5%", "13%", "14%", "13%", "12%", "11%", "")), vjust = -1.5, family = "b612", size = 4)+
  geom_text(aes(x = 1870, y = 93, label = "100%"), color = black, family = "b612", size = 4)+
  geom_text(aes(x = 1830, y = 95, label = "FREE - LIBRE"),
            color = black, family = "jetbrains", size = 9, fontface = "bold")+
  geom_text(aes(x = 1830, y = 40, label = "SLAVES\nESCALVES"), 
            color = tan, family = "jetbrains", size = 9, fontface = "bold")+ 
  scale_x_continuous(limits = c(1790, 1870), 
                     breaks = c(1790, 1800, 1810, 1820, 1830, 1840, 1850, 1860, 1870),
                     position = "top",
                     expand = c(0,0.4))+
  scale_y_continuous(limits = c(0,100), expand = c(0,0))+
  coord_cartesian(clip = "off")+
  labs(y = NULL, x = NULL,
       title = "PROPORTION OF FREEMEN AND SLAVES AMONG AMERICAN NEGROES\nPROPORTION DES NEGRES LIBRES ET DES ESCLAVES EN AMERIQUE",
       subtitle = "DONE BY ATLANTA UNIVERSITY") +
  theme_minimal() +
  theme(panel.grid.major.x = element_line(color = black),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = tan, color = NA),
        panel.background = element_rect(fill = tan, color = NA),
        plot.title = element_text(color = black, family = "jetbrains", hjust = 0.3, vjust = 0, size = 20, face = "bold"),
        plot.subtitle = element_text(color = "black", family = "jetbrains", hjust = 0.5, vjust = 0.5, size = 12),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.text.x = element_text(color = "black", family = "jetbrains", face = "bold", size = 16),
        plot.margin = margin(0.2,0.3,0,0.3, unit = "in"))
