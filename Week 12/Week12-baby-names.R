#ATB
#Tidy Tuesday W12 2022
#Baby names

#load packages
library("tidyverse")
sysfonts::font_add_google(name = "Fredoka", "Fredoka")
showtext::showtext_auto()

#load data
babynames <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-22/babynames.csv')

generations <- babynames %>%
  distinct(., .keep_all = TRUE) %>%
  filter(year %in% c(1900:2020)) %>%
  mutate(Generation = case_when(year %in% c(1946:1964) ~ "Baby Boomer",
                                year %in% c(1900:1924) ~ "GI Generation",
                                year %in% c(1925:1945) ~ "Silent Generation",
                                year %in% c(1965:1979) ~ "Generation X",
                                year %in% c(1980:1994) ~ "Millenials",
                                year %in% c(1995:2020) ~ "Generation Z")) %>%
  group_by(Generation, sex) %>%
  summarize(count = n())

#plot
plot <- ggplot(transform(generations, 
                         Generation = factor(Generation, levels=c("GI Generation", "Silent Generation", "Baby Boomer", "Generation X", "Millenials", "Generation Z"))))+
  geom_bar(aes(x = sex, y = count, fill = Generation), color = "black", stat = 'identity', position = position_dodge(0.9))+
  xlab("")+
  ylab("Number of Unique Names")+
  scale_fill_manual(values = c("#440154FF", "#404788FF", "#2D708EFF",
                        "#29AF7FFF", "#73D055FF", "#FDE725FF"))+
  theme_minimal()+
  coord_polar()+
  labs(title = "How Unique Are Baby Names Across Generations?",
       subtitle = "The number of unique baby names has increased across every\ngeneration for the past century.",
       caption = "Data: Hadley Wickham | Visualization: A Bisesi")+
  theme(axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        text=element_text(size=16,  family="Fredoka"))

