#ATB
#Tidy Tuesday W16 2022
#Crossword clue sentiments

#load packages
library("tidyverse")
library("showtext")
library("janitor")
library("tidytext")
library("textdata")
library("lubridate")

#load data
tuesdata <- tidytuesdayR::tt_load('2022-04-19')
times <- tuesdata$times
dave <- tuesdata$big_dave

#sentiments data
data(stop_words)
afinn_df <- get_sentiments("afinn")

#merge sentiments data
times_sentiments <- times %>%
  select(puzzle_date, answer) %>%
  unnest_tokens(word, answer) %>%
  anti_join(stop_words, by = "word")

dave_sentiments <- dave %>%
  select(puzzle_date, answer) %>%
  unnest_tokens(word, answer) %>%
  anti_join(stop_words, by = "word")

times_df <- times_sentiments %>%
  inner_join(afinn_df, by = "word") %>%
  group_by(puzzle_date) %>%
  summarise(mean = mean(value, na.rm = T),
            error = sd(value, na.rm = T)) %>%
  mutate(error = case_when(is.na(error) ~ 0,
                           TRUE ~ error)) %>%
  drop_na() %>%
  filter(puzzle_date > dmy("01012015")) %>%
  mutate(type = "New York Times")

dave_df <- dave_sentiments %>%
  inner_join(afinn_df, by = "word") %>%
  group_by(puzzle_date) %>%
  summarise(mean = mean(value, na.rm = T),
            error = sd(value, na.rm = T)) %>%
  mutate(error = case_when(is.na(error) ~ 0,
                           TRUE ~ error)) %>%
  drop_na() %>%
  filter(puzzle_date > dmy("01012015")) %>%
  mutate(type = "Big Dave")

alldata <- rbind(times_df, dave_df)

#plot
font_add_google(name = "Mate SC", family = "mate")
font_add_google(name = "Ubuntu", family = "ubuntu")
showtext_auto()

plot <- alldata %>%
  ggplot(aes(x = puzzle_date, y = mean)) +
  geom_rect(aes(xmin = dmy("01012015"), xmax = dmy("31122021"), ymin = 0, ymax = 5.1), 
            fill = "white") +
  geom_rect(aes(xmin = dmy("01012015"), xmax = dmy("31122021"), ymin = -5.1, ymax = 0), 
            fill = "grey60") +
  geom_point(colour = "black") +
  geom_line(colour = "black") +
  annotate("text", x = dmy("01122021"), y = 4.5, 
           label = "POSITIVE", hjust = 1, 
           family = "mate", colour = "black", size = 6) +
  annotate("text", x = dmy("01122021"), y = -4.5, 
           label = "NEGATIVE", hjust = 1, 
           family = "mate", colour = "black", size = 6) +
  facet_wrap(~type) + 
  coord_cartesian(expand = F) +
  labs(x  = "", 
       y = "Average Sentiment of Answer", 
       title = "Big Dave and the Times Crossword Puzzles", 
       subtitle = "The average sentiment of crossword puzzle answers in Big Dave's Crossword Club and the New York Times from 2015 to 2022. \nThe average crossword answer for both the Times and Big Dave's has a slightly negative sentiment.", 
       caption = "Data: Cryptic Crossword Clues | Visualization: A Bisesi") +
  geom_hline(yintercept=0, color="red", linetype = "dashed")+
  theme(plot.margin = unit(c(0.5, 1.2, 0.5, 0.5), "cm"), 
        panel.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.background = element_rect(fill = "#FAFAFA", colour = "#FAFAFA"), 
        plot.title = element_text(family = "mate", hjust = 0.5, size = 22, color = "black",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.subtitle = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "black"),
        plot.caption = element_text(family = "ubuntu", hjust = 0.5, size = 12, color = "black"), 
        axis.text = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"), 
        axis.title = element_text(family = "ubuntu", hjust = 0.5, size = 10, color = "black"), 
        axis.ticks.y = element_blank(),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(family = "ubuntu", size = 20))

