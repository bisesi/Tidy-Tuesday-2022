#ATB
#Tidy Tuesday W13 2022
#NCAA sports

#load packages
library("tidyverse")
library("showtext")
library("usefunc")
library("scales")

#load data
import <- tidytuesdayR::tt_load('2022-03-29')
sports <- import$sports

#clean data
data <- sports %>%
  select(sports, partic_men, partic_women, year, exp_men, exp_women) %>%
  drop_na() %>%
  group_by(sports) %>%
  summarise(mean_exp_men = mean(exp_men),
            mean_exp_women = mean(exp_women),
            mean_partic_men = mean(partic_men),
            mean_partic_women = mean(partic_women)) %>%
  mutate(mean_exp_diff = mean_exp_men - mean_exp_women,
         mean_partic_diff = mean_partic_men - mean_partic_women)%>%
  select(sports,mean_exp_diff, mean_partic_diff) %>%
  mutate(sports = fct_reorder(sports, mean_exp_diff),
         more_women = as.factor(mean_partic_diff < 0)) %>%
  mutate(more_women = case_when(more_women == FALSE ~ "More participants on men's teams",
                                more_women == TRUE ~ "More participants on women's teams"))


#fonts
sysfonts::font_add_google(name = "Fredoka", "Fredoka")
showtext::showtext_auto()

#plot
plot <- data %>%
  ggplot(aes(x = mean_exp_diff, y = sports, color = more_women)) +
  geom_point(size = 3)+
  geom_segment(aes(yend = sports, xend = 0), size = 1)+
  annotate("segment", y = 1, yend = 17, x = 150000, xend = 150000, colour = "#4b0082",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("segment", y = 18, yend = 31, x = -150000, xend = -150000, colour = "#008080",
           arrow = arrow(ends = "both", angle = 90, length = unit(.2,"cm"))) +
  annotate("text", x = 275000, y = 9, label = "Higher expenditure for women's teams", 
           colour = "#4b0082", family = "Fredoka") +
  annotate("text", x = -275000, y = 24.5, label = "Higher expenditure for men's teams", 
           colour = "#008080", family = "Fredoka") +
  scale_x_continuous(labels = unit_format(unit = "K", scale = 1e-3, sep = ""), 
                     limits = c(-600000, 600000), 
                     expand = c(0, 0))+
  scale_colour_manual("", values = c("#008080", "#4b0082"))+
  labs(x = "Average difference in annual expenditure\nfor male and female sports", 
       y = "",
       title = "Expenditure and Participation in College Sports", 
       subtitle = "While expenditure is fairly equal between men's and women's teams across all sports,\n average number of participants is not.",
       caption = "Data: Equity in Athletics Data Analysis | Visualization: A Bisesi") +
  coord_flip()+
  theme(plot.margin = unit(c(0.8, 0.8, 0.5, 0.8), "cm"), 
        panel.background = element_blank(), 
        plot.background = element_blank(), 
        plot.title = element_text(size = 20),
        plot.subtitle = element_text(lineheight = 1, margin = margin(t = 15, b = 20)),
        axis.text.x = element_text(angle = 90, hjust = 1),
        text=element_text(family = "Fredoka"))
