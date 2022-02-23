#ATB
#TidyTuesday W3 2022
#Chocolate Bars

#load packages
library("tidyverse")
library("gt")
library("gtExtras")
library("kableExtra")
library("cowplot")

#load data
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

#keep companies with more than 20 customer reviews
popular <- chocolate %>% 
  count(company_manufacturer, sort=T) %>% 
  filter(n>20)

#summary stats on most popular companies
summarized <- chocolate %>% 
  filter(company_manufacturer %in% popular$company_manufacturer)  %>% 
  group_by(company_manufacturer) %>%
  mutate(avg=mean(rating)) %>%
  ungroup() %>%
  select(c("company_location", "country_of_bean_origin", "cocoa_percent", "avg", "company_manufacturer",  "rating"))

#import paths for logos of most popular companies
table_logos <- tibble(logo = c("/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/soma.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/arete.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/domori.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/bonnat.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/morin.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/fresco.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/zotter.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/valrhona.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/dandelion.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/pralus.png", 
                               "/Users/abisesi/Desktop/PhD/Non-Dissertation Work/Programming Repos/R/Tidy-Tuesday-2022/Week 3/guittard.png"))

#{gt} table
tabledata <- summarized %>%
  group_by(company_location,company_manufacturer) %>%
  summarise(average = round(mean(rating),2),
            min=min(rating),
            median = round(median(rating),2),
            max=max(rating),
            range= max-min,
            histogram=list(rating),
            .groups="drop") %>%
  arrange(desc(average))

withlogos <- cbind(table_logos, tabledata)

table <- withlogos %>%
  select(logo, company_location, company_manufacturer, average, histogram, min, median, max, range) %>%
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
  cols_label(company_manufacturer = html("Manufacturer"), logo = html("")) %>%
  tab_spanner(label="Customer Ratings", 
              columns=c(average:range)) %>%
  tab_header(title=html("Customer Ratings of Plain Dark Chocolate Bar"),
             subtitle=md("Ratings of chocolate bars by manufacturers with more than 20 customer reviews, according to *Flavors of Cacao*"))%>%
  tab_source_note(source_note = gt::html("<br>Data: Flavors of Cacao |  Visualization: A Bisesi")) %>%
  tab_style(style = list(cell_text(weight="lighter")),
    locations = list(cells_title(groups = "subtitle")))
