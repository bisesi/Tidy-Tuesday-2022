#ATB
#Tidy Tuesday W11 2022
#CRAN 

#load packages
library("tidyverse")
library("ggtext")
library("showtext")

#load data
cran <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-15/cran.csv')

#clean data
tidy <- cran %>%
  mutate(date = case_when(grepl("^[[:digit:]]",(substr(date,4,nchar(date)))) ~ as.Date(date), 
                          TRUE ~ as.Date(substr(date,5,nchar(date)), '%b %d %H:%M:%S %Y'))) %>%
  mutate(year = as.numeric(format(date,'%Y'))) %>%
  group_by(package) %>%
  mutate(count = n()) %>%
  ungroup() %>%
  arrange(desc(count))

coremodels <- c("tidymodels", "rsample", "workflows", "broom", 
                "dials", "recipes", "parsnip", "tune", "yardstick")

tidy$releases <- 1

tidymodels <- tidy %>%
  filter(package %in% coremodels) %>%
  drop_na() %>%
  mutate(sticker = paste0("<img src = '/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 11/stickers/", package, ".png' width = '45' </>")) %>%
  group_by(package, year, sticker) %>%
  summarize(yearlyreleases = sum(releases)) %>%
  group_by(package, year) %>%
  mutate(firstrelease = min(year))

#plot
font_add_google("Jetbrains Mono", "jetbrains")
showtext_auto()

plot <- ggplot(tidymodels, aes(y=reorder(sticker,desc(firstrelease)), x=year, fill=yearlyreleases))+
  geom_tile(color="#0D1116", height=0.92, width=0.92)+
  labs(title="TIDYMODELS RELEASES", 
       subtitle="CRAN package releases for tidymodels packages by year",
       y="", x="", fill="Number of Releases",
       caption="Data: CRAN archives & Robert Flight | Visualization: A Bisesi | Code Adapted From: Tanya Shairo")+
  scale_x_continuous(breaks=2014:2021)+
  scale_fill_steps(high='#9BFFC6', low='#008439', na.value="#1A232D",
                   guide=guide_colorsteps(title.vjust=0.8, direction="horizontal"))+
  coord_equal()+
  theme_minimal()+
  theme(panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background = element_rect(fill="#0D1116"),
        panel.background = element_rect(fill="#0D1116"),
        strip.background = element_rect(fill="#0D1116"),
        text=element_text(color="white", family = "jetbrains"),
        plot.title= element_text(face="bold", size = 36, hjust=0.05),
        plot.subtitle=element_text(size = 28, hjust=0.07),
        plot.caption=element_text(size=18),
        axis.text.x=element_text(color="white", size = 22),
        axis.text.y=element_markdown(color='black', size=0.01),
        legend.position = c(0.86,1.03),
        legend.text=element_text(size=22),
        legend.title = element_text(size = 22))

ggsave("tidymodels-timeline-final.png", width=15, height=7.5)

  