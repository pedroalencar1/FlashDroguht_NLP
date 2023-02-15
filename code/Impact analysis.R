"
Prepare impact dataset for analysis and correlate to flash drought occurence

Pedro Alencar

08.02.2023
"


# 0. Import libraries and data --------------------------------------------

library(dplyr)
library(tidyr)
library(terra)
library(tictoc)
library(ggplot2)

# impacts dataset
impact_data <- data.table::fread("data/extracted_impacts_daily_12_12_2022.csv") |>
  select(-V1) |>
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y")))

# total publications per year
number_of_articles_wiso_db <- data.frame(year = seq(2000,2021,1),
                                         articles = c(3727202, 3805202, 4046952, 
                                                      5279895, 6539592, 7031086, 
                                                      7546534, 7769275, 8381606, 
                                                      8416353, 8652610, 9316843, 
                                                      9447050, 9178527, 9121899, 
                                                      9098091, 10399161, 12321421, 
                                                      13537762, 13428658, 11360680, 
                                                      11693705))

impact_data |>
  mutate(year = lubridate::year(date))|>
  filter(type_of_class == "agriculture") |>
  group_by(year) |>
  tally() |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles )|> 
  ggplot(aes(x = year, y = ratio*100000))+
  geom_point()+
  geom_path()+
  labs(x = "Year",
       y = "Ratio")+
  ggtitle("Number of articles with agriculture impacts per 100k")+
  theme_bw()

ggsave("figs/year_articles.png", width = 12, height = 8, units = "cm")


# 1. Impact selection -----------------------------------------------------

impact_fd <- impact_data|>
  filter(type_of_class %in% c(
                              "fire", 
                              "agriculture", 
                              "livestock", 
                              "energy", 
                              "social"
                              )) 

# __Aggreatate into weeks and lvl3 ----------------------------------------

impact_fd_lvl3 <- impact_fd |>
mutate(year = lubridate::year(date),
         week = lubridate::week(date))|>
  group_by(year, week, nuts_id) |>
  tally() |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles,
         ratio_rescale = scales::rescale(ratio)
  ) 


# __Aggreatate into weeks and lvl2 ----------------------------------------

impacts_fd_lvl2 <- impact_fd_lvl3 |>
  ungroup()|>
  mutate(nuts_id = substr(nuts_id, 1, 4))|>
  group_by(year, week, nuts_id) |>
  summarise_at(vars(n), sum) |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles,
         ratio_rescale = scales::rescale(ratio)
  ) 

# 2. Test in single NUTS3 -------------------------------------------------

shape_nuts_path <- "data/GIS/NUTS_RG_20M_2021_4326.shp/NUTS_RG_20M_2021_4326.shp"
shape_nuts <- sf::read_sf(shape_nuts_path) |>
  filter(CNTR_CODE== "DE",
         # LEVL_CODE %in% c(1)
         ) |>
  select(-c(CNTR_CODE,NAME_LATN, MOUNT_TYPE:FID))


impacts_fd_lvl2 |>
  filter(nuts_id == "DE40") |>
  filter(year %in% 2017:2021)|>
  ggplot(aes(x = week, y = ratio, color = factor(year)))+
  geom_path()+
  theme_bw()
  

