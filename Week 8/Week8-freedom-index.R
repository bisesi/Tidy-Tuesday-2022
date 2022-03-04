#ATB
#Tidy Tuesday W8 2022
#Freedom in the world

#practice based on blog post example by Albert Rapp

#load packages
library("tidyverse")
library("ggforce")
library("patchwork")
library("showtext")
library("ggtext")

#load data
freedom <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-02-22/freedom.csv')

#Clean data for 2001 to 2020
countries <- freedom %>%
  dplyr::filter(year %in% c(2001,2020)) %>%
  select(country, Status, year) %>%
  pivot_wider(id_cols = country, names_from = 'year', names_prefix = 'status_', values_from = Status) %>%
  mutate(netchange = case_when(status_2001 == "NF" & status_2020 == "PF" ~ 'improved',
                               status_2001 == "PF" & status_2020 == "F" ~ 'improved',
                               status_2001 == "NF" & status_2020  == "F" ~ 'improved',
                               status_2001 == "F" & status_2020  == "PF" ~ 'declined',
                               status_2001 == "PF" & status_2020  == "NF" ~ 'declined',
                               status_2001 == "F" & status_2020  == "NF" ~ 'declined',
                               TRUE ~ "no change"))
  
regions <- countries %>%
  left_join(freedom %>% filter(year == 2020) %>% select(country, Region_Name)) %>%
  filter(!is.na(status_2001))

data <- regions %>%
  count(status_2001, status_2020, Region_Name, netchange) %>%
  gather_set_data(x = 1:3) %>%
  mutate(y = case_when(y == 'F' ~ "Free",
                       y == 'PF' ~ "Partially Free",
                       y == 'NF' ~ "Not Free",
                       TRUE ~ y)) %>%
  drop_na(x, y)


#Formatting
font_add_google("JetBrains Mono", family = "jetbrains")
showtext_auto()

#Plot
Mainplot <- data %>%
  ggplot(aes(x = factor(x, c('status_2001', 'Region_Name', 'status_2020')),
             split = fct_reorder(y, n, sum, .desc = T),
             id = id,
             value = n)) +
  geom_parallel_sets(aes(fill = netchange, alpha = netchange)) +
  geom_parallel_sets_axes(axis.width = c(rep(0.1, 3), rep(0.3,5), rep(0.1, 3)), col = "black")+
  geom_parallel_sets_labels(color = "white", family = "jetbrains", size = 5, angle = c(rep(-90,3), rep(0,5), rep(-90,3)))+
  annotate('text', x = 1 - (0.1 / 2), y = 230, size = 7, label = "2001",
           family = "Cabin", hjust = 0, fontface = "bold", col = "#333333", alpha = 0.3)+
  annotate('text', x = 3 - (0.1 / 2), y = 230, size = 7, label = "2020",
           family = "Cabin", hjust = 0, fontface = "bold", col = "#333333", alpha = 0.3)+
  theme_void()+
  coord_cartesian(expand = FALSE, xlim = c(0.8, 3.2), ylim = c(-20,240))

improved <- Mainplot +
  scale_fill_manual(values = c('grey80', "#008746", 'grey80')) +
  scale_alpha_manual(values = c(0.3, 1, 0.3))+
  theme_void()
  
declined <- Mainplot +
  scale_fill_manual(values = c("#dc143c", 'grey80', 'grey80')) +
  scale_alpha_manual(values = c(1, 0.3, 0.3))+
  theme_void()

improve_color <- "#008746"
decline_color <- "#dc143c"

patched<- improved + declined &
  theme(text = element_text(family = "jetbrains"),
    plot.title = element_text(size = 26,
      face = 'bold', 
      colour = 'black'),
    plot.subtitle = element_markdown(size = 12,
      face = 'italic', 
      colour = 'black'),
    plot.caption = element_markdown(size = 8),
    legend.position = 'none') & 
  plot_annotation(title = "Freedom in the World Post-9/11",
    subtitle = glue::glue("While the freedom index has <span style = 'color:{improve_color};'>**improved**</span> in some countries since 9/11, many countries have gotten <br><span style = 'color:{decline_color};'>**less free.**</span> Most countries saw <span style = 'color:{'grey80'};'>**no change.**</span>"),
    caption = "Data: Freedom House & UN | Visualization: A Bisesi | Based on blog post by Albert Rapp")
  

