#ATB
#TidyTuesday W2 2022
#Bee colonies

#Load packages
library("tidyverse")
library("showtext")
library("ggtext")
library("cowplot")
library("geojsonio")
library("ggplot2")
library("viridis")
library("rgeos")
library("broom")
library("colorspace")
library("colorblindr")
library("patchwork")

colonies <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/colony.csv')
stressors <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-11/stressor.csv')

colony2020 <- colonies %>% 
  filter(year == 2020) %>%
  group_by(months, state) %>% 
  summarize(total_colonies = sum(colony_n, na.rm = TRUE),
            loss = mean(colony_lost_pct,na.rm = TRUE)) %>%
  mutate(loss = replace_na(loss, 0)) %>%
  mutate(months = case_when(months == "April-June" ~ "Spring",
                            months == "July-September" ~ "Summer",
                            months == "October-December" ~ "Fall",
                            months == "January-March" ~ "Winter")) %>%
  split(.$months)

#hexmap
setwd("/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 2")
spdf <- geojson_read("us_states_hexgrid.geojson",  what = "sp")
spdf@data = spdf@data %>%
  mutate(google_name = gsub(" \\(United States\\)", "", google_name))
spdf_fortified <- tidy(spdf, region = "google_name")
centers <- cbind.data.frame(data.frame(gCentroid(spdf, byid=TRUE), id=spdf@data$iso3166_2))

#merge data
spring <- spdf_fortified %>%
  left_join(., colony2020$Spring, by = c("id" = "state"))
fall <- spdf_fortified %>%
  left_join(., colony2020$Fall, by = c("id" = "state"))
summer <- spdf_fortified %>%
  left_join(., colony2020$Summer, by = c("id" = "state"))
winter <- spdf_fortified %>%
  left_join(., colony2020$Winter, by = c("id" = "state"))

#plot
font_add_google("Cabin")
showtext_auto()

spring_plot <- ggplot() +
  geom_polygon(data = subset(spring, !is.na(loss)), 
               aes(x = long, y = lat, group = group, fill = loss), color = "white")+
  geom_polygon(data = subset(spring, is.na(loss)), 
               aes(x = long, y = lat, group = group), fill = "darkgreen", color = "white")+
  geom_text(data = centers, aes(x = x, y = y, label = id), size = 6, fontface = c("bold"), color = "white", family = "Cabin") +
  theme_void()+
  scale_fill_viridis(limits = c(0,50), option = "viridis")+
  coord_map()+
  ggtitle(label = "SPRING")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Cabin", size = 30))+
  guides(fill=guide_legend(title="Percent of\nColonies Lost"))

fall_plot <- ggplot() +
  geom_polygon(data = subset(fall, !is.na(loss)), 
               aes(x = long, y = lat, group = group, fill = loss), color = "white")+
  geom_polygon(data = subset(fall, is.na(loss)), 
               aes(x = long, y = lat, group = group), fill = "darkgreen", color = "white")+
  geom_text(data = centers, aes(x = x, y = y, label = id), size = 6, fontface = c("bold"), color = "white", family = "Cabin") +
  theme_void()+
  scale_fill_viridis(limits = c(0,50), option = "viridis")+
  ggtitle(label = "FALL")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Cabin", size = 30))+
  coord_map()+
  guides(fill=guide_legend(title="Percent of\nColonies Lost"))

winter_plot <- ggplot() +
  geom_polygon(data = subset(winter, !is.na(loss)), 
               aes(x = long, y = lat, group = group, fill = loss), color = "white")+
  geom_polygon(data = subset(winter, is.na(loss)), 
               aes(x = long, y = lat, group = group), fill = "darkgreen", color = "white")+
  geom_text(data = centers, aes(x = x, y = y, label = id), size = 6, fontface = c("bold"), color = "white", family = "Cabin") +
  theme_void()+
  scale_fill_viridis(limits = c(0,50), option = "viridis")+
  coord_map()+
  ggtitle(label = "WINTER")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Cabin", size = 30))+
  guides(fill=guide_legend(title="Percent of\nColonies Lost"))

summer_plot <- ggplot()+
  geom_polygon(data = subset(summer, !is.na(loss)), 
               aes(x = long, y = lat, group = group, fill = loss), color = "white")+
  geom_polygon(data = subset(summer, is.na(loss)), 
               aes(x = long, y = lat, group = group), fill = "darkgreen", color = "white")+
  geom_text(data = centers, aes(x = x, y = y, label = id), size = 6, fontface = c("bold"), color = "white", family = "Cabin")+
  theme_void()+
  ggtitle(label = "SUMMER")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold", family = "Cabin", size = 30))+
  scale_fill_viridis(limits = c(0,50), option = "viridis")+
  coord_map()+
  guides(fill=guide_legend(title="Percent of\nColonies Lost"))

summer_desaturate <- edit_colors(summer_plot, desaturate)
spring_desaturate <- edit_colors(spring_plot, desaturate)
winter_desaturate <- edit_colors(winter_plot, desaturate)
fall_desaturate <- edit_colors(fall_plot, desaturate)

#final plot
patched <- ggdraw(spring_desaturate) + ggdraw(summer_desaturate) + ggdraw(fall_desaturate) + ggdraw(winter_desaturate) + 
  plot_layout(guides = "collect") & theme(text = element_text(family = "Cabin")) &
  plot_annotation(title = "Loss of bee colonies in the US in 2022",
                  caption = "Data: USDA | Visualization: A Bisesi",
                  theme = theme(plot.background = element_rect(fill  = "#F6BE00")))&
  theme(title = element_text(size = 32))

#export
ggsave('bee_colonies.png', patched)

