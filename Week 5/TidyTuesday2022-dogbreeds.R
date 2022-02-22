#ATB
#TidyTuesday W5 2022
#Dog Breeds

library(tidytuesdayR)
library(tidyverse)
library(ggradar)
library(scales)
library(showtext)
library(stringr)
library(cowplot)

#import dataset
breed_traits <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_traits.csv') %>%
  mutate(Breed = str_squish(Breed)) %>% .[c(1:4, 10:14)]
trait_description <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/trait_description.csv')
breed_rank_all <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-01/breed_rank.csv') %>%
  mutate(Breed = str_squish(Breed)) %>% select(Breed, "2020 Rank")

#create data for plot
data<-breed_traits%>%
  left_join(breed_rank_all, by = "Breed") %>%
  head(10) %>%
  select("Breed","Affectionate With Family", "Good With Young Children", 
         "Watchdog/Protective Nature","Playfulness Level","Trainability Level",
         "Adaptability Level")%>%
  rename(Affection = "Affectionate With Family",
         Playfulness = "Playfulness Level",
         Gentleness = "Good With Young Children",
         Protectiveness = "Watchdog/Protective Nature",
         Trainability = "Trainability Level",
         Adaptability = "Adaptability Level") %>%
  mutate(Breed = str_squish(Breed),
          Breed = case_when(Breed == "Retrievers (Labrador)" ~ "Labrador\nRetriever",
                           Breed == "French Bulldogs" ~ "French\nBulldog",
                           Breed == "German Shepherd Dogs" ~ "German\nShepherd",
                           Breed == "Retrievers (Golden)" ~ "Golden\nRetriever",
                           Breed == "Bulldogs" ~ "Bulldog",
                           Breed == "Poodles" ~ "Poodle",
                           Breed == "Beagles" ~ "Beagle",
                           Breed == "Rottweilers" ~ "Rottweiler",
                           Breed == "Pointers (German Shorthaired)" ~ "Short\nHaired\nPointer",
                           Breed == "Dachshunds" ~ "Dachshund"))

#create factor to help order ggplot facet (will default to alphabetical)
data$Breed<-factor(data$Breed, levels=data$Breed)

#get names of columns
names(data)[1]<-"group"

#radar plot
Mainplot <- data%>%
  ggradar(
    grid.min = 1, grid.mid = 3, grid.max = 5,
    background.circle.colour = "white",
    values.radar = c("1", "3","5"),
    group.point.size = 1,
    gridline.min.linetype = 1,
    gridline.mid.linetype = 1,
    gridline.max.linetype = 1,
    axis.line.colour = "grey",
    gridline.min.colour = "grey",
    gridline.mid.colour = "grey",
    gridline.max.colour = "grey",
    group.line.width = 1,
    grid.label.size=5,
    axis.label.size=3,
    plot.extent.x.sf = 1.5,
    plot.extent.y.sf = 1.5,
    group.colours = "#117733",
    font.radar = "Montserrat"
  )+
  facet_wrap(vars(group), ncol = 5)+
  labs(title="Personalities of The Most Popular Dog Breeds",
       subtitle="Personality traits scaled from 1 to 5",
       caption="Data: American Kennel Club | Visualization: A Bisesi")+
  theme_void()+
  theme(legend.position="none",
        plot.background = element_rect(fill = "white"),
        plot.subtitle= element_text(family = "Montserrat", hjust=0.5, margin=margin(0,0,30,0)),
        plot.caption=element_text(family = "Montserrat", size=10,  face = "italic"),
        strip.text = element_text(size = 10,  family = "Montserrat", face = "bold"),
        plot.margin=margin(20,0,10,0),
        plot.title=element_text(family = "Montserrat", hjust=0.5, face="bold", size=18, margin=margin(0,0,10,0)))

#Annotated plot
ggdraw(Mainplot) +
  draw_image("/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/AKC.jpeg",
             x = 0.9, y = 0.1, hjust = 1, vjust = 1, 
             halign = 1, valign = 1, width = 0.1)
