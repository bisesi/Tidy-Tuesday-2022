#ATB
#Tidy Tuesday W19 2022
#NYT bestsellers

#import packages
library("tidyverse")
library("ggbump")

#import data
nyt_titles <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_titles.tsv')
nyt_full <- readr::read_tsv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-10/nyt_full.tsv')

#calculate movement in rankings from week to week
clean_titles <- nyt_titles %>% 
  slice_max(total_weeks, n=20) %>%
  mutate(title=str_to_title(title)) 

rankmovements <- nyt_full %>% 
  filter(title_id %in% clean_titles$id) %>%
  mutate(rank=-1*rank) %>%
  group_by(id=title_id) %>%
  arrange(week) %>%
  summarise(timeline=list(rank),.groups="drop")

fulldata <- clean_titles %>% left_join(rankmovements, by="id")

rankeddata <- nyt_full %>%
  filter(year==2019) %>%
  group_by(title_id) %>%
  mutate(n=n())

selectedworks <- rankeddata %>% 
  filter(rank==1) %>% 
  count(title_id, sort=T) %>% 
  filter(n>1) %>% 
  pull(title_id)

labels <- rankeddata %>% 
  filter(title_id %in% selectedworks) %>% 
  group_by(title_id) %>% 
  filter(week==min(week))

#add fonts
font_add_google("Source Sans Pro", "pro")
showtext_auto()

#make plot
plot <- rankeddata %>%
  ggplot(aes(x=week, y=rank)) +
  geom_point(color="grey", alpha=.8, size=.5) +
  geom_bump(aes(group=title_id), alpha=.7, size=.6, color="grey80") +
  geom_bump(data= rankeddata %>% filter(title_id %in% selectedworks), 
                    aes(group=title_id, color=title), 
                    alpha=.8, size=1, show.legend = F) +
  geom_point(data= rankeddata %>% filter(title_id %in% selectedworks),
             size=.7, shape=21, fill="white", color="black", stroke=.3) +
  geom_text(data= labels %>% filter(title_id %in% c(6105)), 
            aes(y=0.6,label=str_to_title(title), color=title), 
            size=4, show.legend=F, family="pro", hjust=0, fontface="bold") +
  geom_text(data=labels %>% filter(title_id %in% c(7239, 6649)), 
            aes(y=0.1,label=str_to_title(title),color=title), 
            size=4, show.legend=F, family="pro", hjust=0, fontface="bold") +
  scale_y_reverse(breaks=seq(1,15,1), expand=c(0.02,0.02),
                  labels=c("Rank 1","2","3","4","5","6","7","8","9","10","11","12","13","14","15")) +
  coord_cartesian(clip="off") +
  theme_minimal() +
  theme(text=element_text(family="pro"),
        legend.position = "top",
        plot.subtitle = element_text(size=12.5, margin=margin(b=10)),
        plot.title.position = "plot",
        plot.title=element_text(family="pro", face="bold", size=15.5),
        panel.grid=element_blank(),
        axis.title=element_blank(),
        axis.ticks=element_line(color="grey50", size=.4),
        axis.ticks.length=unit(.25, "cm"),
        axis.text=element_text(color="black"),
        plot.caption=element_text(hjust=0, color="grey50", size=11),
        plot.margin=margin(.5,.9,.3,.35, unit="cm"),
        plot.caption.position = "plot") +
  labs(title="New York Times Bestsellers in 2019",
       subtitle="Titles ranking number one for two or more weeks on The New York Times fiction bestseller list",
       caption="Visualization: A Bisesi | Data: Post45 Data, Sara Stoudt")

