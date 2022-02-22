#ATB
#TidyTuesday W3 2022
#Chocolate Bars

library(tidyverse)
library(gt)
library(gtExtras)
library(kableExtra)
library(cowplot)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

#more than 20 reviews
popular <- chocolate %>% 
  count(company_manufacturer, sort=T) %>% 
  filter(n>20)

#summarize most popular companies
summarized <- chocolate %>% 
  filter(company_manufacturer %in% popular$company_manufacturer)  %>% 
  group_by(company_manufacturer) %>%
  mutate(avg=mean(rating)) %>%
  ungroup() %>%
  select(c("company_location", "country_of_bean_origin", "cocoa_percent", "avg", "company_manufacturer",  "rating"))

#table logos
table_logos <- tibble(logo = c("/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png", 
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/domori.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png", 
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/zotter.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/valrhona.png",
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/dandelion.png", 
                 "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/pralus.png",
                "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Visualizations/Tidy Tuesday/guittard.png"))

#{gt} table
table <- summarized %>%
  group_by(company_location,company_manufacturer) %>%
  summarise(average = round(mean(rating),2),
            min=min(rating),
            median = round(median(rating),2),
            max=max(rating),
            range= max-min,
            histogram=list(rating),
            .groups="drop") %>%
  arrange(desc(average)) %>%
  cbind(table_logos) %>%
  select(company_location, company_manufacturer, average, histogram, min, median, max, range, logo) %>%
  gt() %>%
  gt_theme_538() %>%
  gt_sparkline(
    histogram,
    type = "histogram",
    line_color = "#66462c",
    fill_color = "#66462c",
    bw = .25,
    same_limit = TRUE) %>%
  gt_merge_stack(company_manufacturer, company_location, colors=c("#38160d","grey")) %>%
  gt_color_rows(columns = c("average","range"),
                palette = "ggsci::brown_material") %>%
  gt_img_rows(columns = logo, img_source = "local", height = 25) %>%
  cols_align(columns = c("histogram", "median"),
             align="center") %>%
  cols_label(company_manufacturer = html("Manufacturer")) %>%
  tab_spanner(label="Customer Ratings", 
              columns=c(average:range)) %>%
  tab_header(title=html("Customer Ratings of Plain Dark Chocolate Bar"),
             subtitle=md("Ratings of chocolate bars by manufacturers with more than 20 customer reviews, according to *Flavors of Cacao*"))%>%
  tab_source_note(source_note = gt::html("<br>Data: Flavors of Cacao |  Visualization: A Bisesi")) %>%
  tab_style(style = list(cell_text(weight="lighter")),
    locations = list(cells_title(groups = "subtitle")))  

