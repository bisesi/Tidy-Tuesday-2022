#ATB
#Tidy Tuesday W18 2022
#Wind and solar utilities

#load packages
library("tidyverse")
library("showtext")

#import data
wind <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/wind.csv')
solar <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-03/solar.csv')

#clean data for projected cost
pivot_solar <- solar %>% 
  rename("Solar projected price in $/MWh"=solar_mwh, "Solar projected capacity in Gigawatts"=solar_capacity) %>% 
  pivot_longer(2:3) %>% 
  mutate(grp="Solar")
pivot_wind <- wind %>% 
  rename("Wind projected price in $/MWh"=wind_mwh, "Wind projected capacity in Gigawatts"=wind_capacity) %>% 
  pivot_longer(2:3) %>% 
  mutate(grp="Wind") 
alldata <- rbind(pivot_solar, pivot_wind) %>%
  rename(Projected = name)

#import fonts for plot
font_add_google("Barlow", "barlow")
showtext_auto()

plot <- alldata %>%
  ggplot(aes(x=date, y=value, color=Projected, fill=Projected)) +
  geom_point(shape=21, size=.7, key_glyph = draw_key_rect, alpha=.75) +
  geom_smooth(show.legend=F) +
  scale_x_date(expand=c(0,0)) +
  facet_wrap(~grp, ncol=2) +
  theme_minimal() +
  theme(legend.position = "top",
        text=element_text(family="barlow"),
        legend.justification = "center",
        plot.title.position = "plot",
        plot.title=element_text(hjust=.5, size=rel(1.55), margin=margin(b=10)),
        plot.margin=margin(.2,.3,.2,.2, unit="cm"),
        panel.spacing = unit(1.5, "lines"),
        strip.text=element_text(face="bold", size=rel(1)),
        axis.title=element_blank()) +
  guides(color=guide_legend(nrow=2))+
  labs(caption="Visualization: A Bisesi | Data: Berkeley Lab")
