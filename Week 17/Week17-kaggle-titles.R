#ATB
#Tidy Tuesday W17 2022
#Kaggle

#load packages
library("tidyverse")
library("showtext")
library("tidytext")
library("lubridate")

#load data
hidden_gems <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-04-26/hidden_gems.csv')

#clean data
clean_title <- hidden_gems %>%
  select(vol, date, title) %>%
  distinct(vol, date, title) %>%
  unnest_tokens(word, title) %>% 
  anti_join(get_stopwords()) %>%
  mutate(stem = wordStem(word))%>%
  count(stem, sort=T) %>%
  filter(n > 5)

clean_review <- hidden_gems %>%
  select(vol, date, review) %>%
  distinct(vol, date, review) %>%
  mutate(review = gsub(r"{\s*\([^\)]+\)}","",as.character(review)),
         review = gsub("\\[|\\]", "", review)) %>%
  unnest_tokens(word, review) %>% 
  anti_join(get_stopwords()) %>%
  mutate(stem = wordStem(word)) %>% 
  count(stem, sort=T) %>%
  filter(n > 5)

allstems <- clean_title %>%
  inner_join(., clean_review, by = "stem") %>%
  mutate(count = n.x + n.y) %>%
  arrange(desc(count))

#plot
font_add_google(name = "Mate SC", family = "mate")
font_add_google("Merriweather", "Merriweather")
showtext_auto()

plot <- allstems %>%
  ggplot(aes(x = reorder(stem, -count, sum), y = count)) +
  geom_point() +
  labs(x  = "Word Stems", 
       y = "Number of Mentions", 
       title = "Kaggle Project Titles and Summaries", 
       subtitle = "The most common word stems in Kaggle project titles and summaries \nrelate to visualization and exploratory data analysis.", 
       caption = "Data: Kaggle (Martin Henze) | Visualization: A Bisesi") +
  theme(plot.margin = unit(c(0.5, 1.2, 0.5, 0.5), "cm"), 
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.title = element_text(family = "mate", hjust = 0.5, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "Merriweather", hjust = 0.5, size = 12, color = "black"),
        plot.caption = element_text(family = "Merriweather", hjust = 0.5, size = 12, color = "black"), 
        axis.text = element_text(family = "Merriweather", hjust = 0.5, size = 10, color = "black"), 
        axis.title = element_text(family = "Merriweather", hjust = 0.5, size = 10, color = "black"), 
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(family = "Merriweather", size = 20))
  
