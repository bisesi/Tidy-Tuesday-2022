#ATB
#TidyTuesday W24 2022
#US drought levels

#load packages
library("geofacet")
library("ggnewscale")
library("showtext")
library("tidyverse")

#import data
drought <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought.csv')
drought_fips <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-06-14/drought-fips.csv')

#prep data for plot
data.frame <- drought %>%
  mutate(DATE=str_remove(DATE,"d_"),
         date=lubridate::ymd(DATE),
         year=lubridate::year(date)) %>%
  filter(year>=2000) %>%
  mutate(state=str_to_title(gsub("-", " ", state)),
         abbrev = state.abb[match(state,state.name)])

range(data.frame$date)

dry <- data.frame %>% select(state, abbrev, date, 3:7) %>%
  pivot_longer(!1:3) #drier as positive values

wet <- data.frame %>% select(state, abbrev, date, 9:13) %>%
  pivot_longer(!1:3) %>%
  mutate(value=value*-1) #set wet as negative values

# Grid geofacet
mapUS <- us_state_grid1 %>% filter(!code %in% c("DC","HI","AK"))

#prep for plot
font_add_google("IBM Plex Sans", "ibm")
showtext_auto()

#plot
plot <- dry %>% 
  ggplot() +
  geom_col(data = dry, aes(x = date, y = value, fill = name)) +
  scale_fill_brewer("",palette = "OrRd", labels=c("Abnormally dry","Moderate drought","Severe drought","Extreme drought","Exceptional drought")) +
  guides(fill=guide_legend(nrow=2)) +
  ggnewscale::new_scale_fill() +
  geom_col(data=wet, aes(x = date, y = value, fill = name)) +
  scale_fill_brewer("",palette = "Blues", labels=c("Abnormally wet","Moderate wet","Severe wet","Extreme wet","Exceptional wet")) +
  guides(fill=guide_legend(nrow=2)) +
  scale_x_date(date_labels = "'%y") +
  facet_geo(~abbrev, grid = mapUS , label = "name") +
  cowplot::theme_minimal_grid(9.5) +
  theme(text=element_text(color = "black", family = "ibm"),
        panel.grid = element_blank(),
        legend.position = "top",
        axis.text.y=element_blank(),
        axis.text.x = element_text(color="black", size=rel(.7)),
        axis.title=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x = element_line(color="black", size=.2),
        plot.title=element_text(size=rel(1.2), hjust=0.5),
        plot.subtitle = element_text(hjust=0.5, margin=margin(b=8)),
        strip.text = element_text(size=rel(.7)),
        legend.justification = "center",
        plot.caption=element_text(hjust=0, color = "black"),
        plot.margin=margin(.5,.5,.5,.5, unit= "cm")) +
  labs(caption="\nData: National Integrated Drought Information System | Visualization: A Bisesi",
       title="Drought Conditions in the US",
       subtitle="January 01 2000 to April 01 2022")      
