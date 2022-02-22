#ATB
#Tidy Tuesday W4 2022
#Board Games

library(tidyverse)
library(showtext)
library(ggplot2)
library(ggrepel)

ratings <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/ratings.csv')
details <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-25/details.csv')

detailed_ratings <- ratings %>%
  select(id, rank, average, year, name, users_rated) %>%
  left_join(details, by = "id") %>%
  select(-c(id, boardgameimplementation, boardgameexpansion, boardgamedesigner, boardgamefamily,
            description, primary, yearpublished, minage, boardgameartist, boardgamepublisher, 
            owned, trading, wanting, num, wishing, playingtime, boardgamemechanic)) %>%
  filter(rank <= 100 & users_rated > mean(users_rated)) %>%
  mutate(playtime = (minplaytime + maxplaytime) / 2) %>%
  mutate(players = round((minplayers + maxplayers) / 2)) %>%
  arrange(rank)

#clean categories 
categories <- c()
for (i in 1:nrow(detailed_ratings)){
  game_categories <- unlist(detailed_ratings[i,10])
  remove_apos <- gsub("\\'", "", game_categories)
  remove_bracka <- gsub("\\[", "", remove_apos)
  remove_brackb <- gsub("\\]", "", remove_bracka)
  split_categories <- str_split(remove_brackb, ", ")
  for (item in split_categories){
    categories <- c(categories, item)
  }
}

categories <- as.data.frame(table(categories))

sorted_categories <- categories %>%
  arrange(desc(Freq)) %>%
  head(n = 6)

#rename categories
subset <- detailed_ratings %>%
  mutate(category = case_when(
    str_detect(boardgamecategory, "Adventure") & str_detect(boardgamecategory, "Fighting") ~ "Adventure & Fighting",
    str_detect(boardgamecategory, "Fantasy") & str_detect(boardgamecategory, "Science Fiction") ~ "Fantasy & Science Fiction",
    str_detect(boardgamecategory, "Fantasy") ~ "Fantasy & Science Fiction",
    str_detect(boardgamecategory, "Science Fiction") ~ "Fantasy & Science Fiction",
    str_detect(boardgamecategory, "Adventure") ~ "Adventure & Fighting",
    str_detect(boardgamecategory, "Fighting") ~ "Adventure & Fighting",
    str_detect(boardgamecategory, "Card Game") ~ "Card Game",
    str_detect(boardgamecategory, "Economic") ~ "Economic")) %>%
  filter(is.na(category)==FALSE & playtime < 112.50 & year > 2010) %>%
  select(-c(rank, minplaytime, maxplaytime, users_rated, boardgamecategory, minplayers, maxplayers))


#average rating, playtime,maxplayers, category
font_add_google("Cabin")
showtext_auto()

plot <- subset %>%
  ggplot(aes(x = playtime, y = average))+
  geom_point(aes(color = category, fill = category), size = 4, alpha = 0.7)+
  scale_color_manual(values=c("#095CA0", "purple", "#1A831F", "red")) +
  scale_fill_manual(values=c("#095CA0", "purple", "#1A831F", "red")) +
  geom_label_repel(aes(label=name), family = "Cabin", size = 3)+
  facet_wrap(~category, nrow=2)+
  ggplot2::labs(title="Board Game Ratings (2010-2020)",
       caption="\nData: Kaggle  | Visualization: A Bisesi")+
  ylab("Average Rating")+
  xlab("Average Play Time (minutes)")+
  theme_minimal()+
  theme(
    plot.title = element_text(color="white", family="Cabin", size=24, hjust = 0.5),
    plot.caption = element_text(color="#333333", family="Cabin", size=12, hjust=0.5),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.background = element_rect(fill="black"),
    panel.background = element_rect(fill="black", color="#333333"),
    panel.grid = element_line(color="#333333"),
    axis.text = element_text(color="white", family="Cabin", size=16),
    axis.title.x = element_text(color = "white", family = "Cabin", size = 16),
    axis.title.y = element_text(color="white", family="Cabin", size=16),
    axis.ticks = element_blank(),
    strip.background = element_rect(fill="black"),
    strip.text = element_text(color="white", family="Cabin", size=16),
    legend.position = 'none')
