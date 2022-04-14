#ATB
#Tidy Tuesday W15 2022
#Indoor air pollution

#load packages
library("tidyverse")
library("showtext")
library("janitor")
library("ggrepel")

#import data
data <- tidytuesdayR::tt_load(2022, week = 15)
fuel_gdp <- data$fuel_gdp
indoor_pollution <- data$indoor_pollution

fuel_gdp <- 
  fuel_gdp %>%
  clean_names() %>%
  rename(access_clean_fuel = starts_with("access_to"),
         gdp_pc = starts_with("gdp_per"),
         pop = starts_with("population"),
         country = entity)

indoor_pollution<- 
  indoor_pollution %>% 
  clean_names() %>% 
  rename(country = entity, 
         death = starts_with("deaths"))

all_data <- 
  fuel_gdp %>% 
  left_join(indoor_pollution)

#plot
font_add_google("Merriweather", "Merriweather")
showtext_auto()

plot <- all_data %>%
  filter(year == 2015) %>%
  drop_na() %>%
  filter(continent == "Africa") %>%
  ggplot(aes(x = death, y = access_clean_fuel, color = country)) +
  geom_point(position = position_dodge(width = 2), 
             alpha = 0.3, aes(size = gdp_pc)) +
  geom_text_repel(aes(label = country), max.overlaps = Inf) +
  scale_y_continuous(limits = c(0, 100),
                     labels = function(y) paste0(y, "%")) +
  scale_x_continuous(limits = c(0, 20),
                     labels = function(x) paste0(x, "%")) +
  scale_size(range = c(0.1, 20)) +
  coord_cartesian(clip = "off") +
  scale_color_viridis_d(option = "B") +
  theme(plot.margin = margin(rep(20, 4)),
        plot.title = element_text(color = "#264653", 
                                  family = "Merriweather", size = 22),
        plot.subtitle = element_text(margin = margin(b=20), 
                                     family = "Merriweather", size = 14,
                                     color = "#14213d"),
        plot.title.position = "plot",
        plot.caption = element_text(size = 12, 
                                    color = "#264653", 
                                    margin = margin(t = 10)),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.title.x = element_text(margin = margin(t=15), 
                                    size = 12,
                                    family = "Merriweather",
                                    color = "#14213d"),
        axis.title.y = element_text(margin = margin(r=15), 
                                    size = 12,
                                    family = "Merriweather",
                                    color = "#14213d"),
        legend.position = "none") +
  labs(title = "Access to clean fuels for cooking reduces indoor air pollution death rates, 2015",
       subtitle = "Bubble size corresponds to GDP per capita for African countries",
       y = "Percent of population \nwith access to clean fuels for cooking",
       x = "Death rates as a result of indoor air pollution",
       caption = "Data: Our World in Data | Visualization: A Bisesi")

