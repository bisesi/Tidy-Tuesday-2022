#ATB
#Tidy Tuesday W9 2022
#EV charging stations

#load packages
library("tidyverse")
library("sf")
library("maps")
library("ggspatial")
library("rnaturalearth")
library("showtext")
library("ggtext")

#load data
stations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-03-01/stations.csv')

stations <- stations %>%
  mutate(FUEL_TYPE = case_when(FUEL_TYPE_CODE == "CNG" | FUEL_TYPE_CODE == "LNG" ~ "Natural Gas",
                               FUEL_TYPE_CODE == "BD" ~ "Biodiesel",
                               FUEL_TYPE_CODE == "ELEC" ~ "Electric",
                               FUEL_TYPE_CODE == "E85" ~ "Ethanol",
                               FUEL_TYPE_CODE == "HY" ~ "Hydrogen",
                               FUEL_TYPE_CODE == "LPG" ~ "Propane",
                               TRUE ~ FUEL_TYPE_CODE))

citycounts <- stations %>%
  select(c(LONGITUDE, LATITUDE, CITY, STATE, FUEL_TYPE)) %>%
  group_by(CITY, FUEL_TYPE) %>%
  count(CITY, FUEL_TYPE)

locations <- citycounts %>%
  full_join(stations) %>%
  select(CITY, n, LONGITUDE, LATITUDE, STATE, FUEL_TYPE) %>%
  distinct(CITY, .keep_all = TRUE)

positions <- locations  %>%
  filter(!STATE %in% c("DC","PR", "HI", "AK", "AS", "VI", "GU", "MP"))

chargetype <- split(positions, positions$FUEL_TYPE)
  
#get state data
state_map_data <- map('state', fill = TRUE, plot = FALSE) %>% st_as_sf()
  
#Formmating
font_add_google("Cabin")
showtext_auto()

#Plot
plot <- ggplot() +
  geom_sf(data = state_map_data, fill = "white") +
  geom_jitter(data = positions, aes(x = LONGITUDE, y = LATITUDE, color = FUEL_TYPE), size = 1)+
  scale_color_viridis("Fuel Type", option = "plasma", discrete = TRUE)+
  labs(title = "Renewable Energy Access",
       subtitle = "Density of alternative transportation fuel stations in the continental US",
       caption = "Data: US DOT | Visualization: A Bisesi",
       fill = "Fuel Type")+
  theme(text = element_text(family = 'Cabin'),
        panel.grid.major = element_line(color = 'white',
                                        linetype = 'dashed',
                                        size = .3),
        panel.background = element_rect(fill = 'white'),
        plot.title = element_text(size = 32),
        plot.subtitle = element_text(size = 16),
        axis.title = element_blank(),
        axis.text = element_blank(), 
        axis.ticks = element_blank(),
        legend.text = element_text(size = 14),
        legend.title = element_text(size = 14))+
   guides(colour = guide_legend(override.aes = list(size=5)))

