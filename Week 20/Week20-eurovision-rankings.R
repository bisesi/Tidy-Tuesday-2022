#ATB
#Tidy Tuesday W20 2022
#Eurovision

#import packages
library("tidyverse")
library("showtext")

#import data
eurovision <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-17/eurovision.csv')

#clean data
cleaned_eurovision <- eurovision %>% 
  filter(year!=2020, section %in% c("final", "grand-final")) %>%
  group_by(year) %>%
  mutate(last= case_when(rank==max(rank)~1, TRUE~0),
         n_host = case_when(host_country==artist_country~1, TRUE~0)) %>%
  ungroup() %>%
  group_by(country_emoji, artist_country) %>%
  summarise(n=n(),
            n_winner=length(n[winner=="TRUE"]),
            n_last = sum(last),
            highest_rank=min(rank),
            min_year=min(year),
            max_year=max(year),
            n_host= sum(n_host)) %>%
  ungroup()

grandfinals <- eurovision %>% 
  filter(section=="grand-final", year!=2020) %>% #no grand finals in 2020 due to COVID
  group_by(artist_country) %>%
  mutate(n=n_distinct(year))

notable_rankings <- grandfinals %>% 
  filter(rank %in% c(1,2,3))

bottom_rankings <- grandfinals %>% 
  group_by(year) %>% 
  slice_max(rank,n=1)  

#import fonts
font_add_google("Karla", "karla")
showtext_auto()

#plot
plot <- grandfinals %>%
  ggplot(aes(x=year, y=	fct_rev(artist_country))) +
  geom_line(aes(group=fct_rev(artist_country)), size=.3, color="grey70") +
  geom_text(data=grandfinals %>% select(artist_country, n) %>% distinct(),
            aes(x=2022.5, y=fct_rev(artist_country), label=glue::glue("n= {n}")), 
            family="karla", color="grey50", size=3.5, hjust=0) +
  geom_point(shape=21, size=2.5, fill="white") +
  geom_point(data=notable_rankings, aes(fill=rank_ordinal), size=2.5, shape=21) +
  geom_point(data=bottom_rankings, aes(fill="Last"), size=2.5, shape=21) +
  scale_x_continuous(position="top", breaks=seq(2005,2020,5),
                     expand = expansion(mult = c(.02, NA))) +
  coord_cartesian(clip = "off") +
  theme_minimal() +
  theme(text=element_text(family="karla"),
        legend.title = element_blank(),
        axis.title=element_blank(),
        axis.line=element_blank(),
        axis.ticks.y=element_blank(),
        legend.position = "top",
        plot.margin=margin(.4,1.4,.3,.4, unit="cm"),
        plot.title.position = "plot",
        plot.title=element_text(size=13),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust=0, color="grey20", margin=margin(t=13), size=9)) +
  labs(title="Eurovision Grand Final Rankings",
       subtitle="From 2004 to 2022, arranged in alphabetical order by artist's country of origin",
       caption="Visualization: A Bisesi | Data: Eurovision")
