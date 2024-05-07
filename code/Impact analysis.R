"
Prepare impact dataset for analysis and correlate to flash drought occurence

Pedro Alencar

08.02.2023
"


# 0. Import libraries and data --------------------------------------------

library(dplyr)
library(tidyr)
library(magrittr)
library(terra)
library(tictoc)
library(ggplot2)
library(terra)
library(plotly)
library(sf)

source("../../@R scripts/Utilities.R")
source("code/functions.R")

# impacts dataset
impact_data <- data.table::fread("data/extracted_impacts_daily_12_12_2022.csv") |>
  select(-V1) |>
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y"))) |>
  select(-nuts_name)

impact_data_2022 <- data.table::fread("data/impacts_daily_2022/pedro_export_2022_b.csv") |>
  select(-V1) |>
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y"))) |>
  select(nuts_id, date, type_of_class, id)
  
impact_data <- rbind(impact_data, impact_data_2022)

# total publications per year
number_of_articles_wiso_db <- data.frame(year = seq(2000,2022,1),
                                         articles = c(3727202, 3805202, 4046952, 
                                                      5279895, 6539592, 7031086, 
                                                      7546534, 7769275, 8381606, 
                                                      8416353, 8652610, 9316843, 
                                                      9447050, 9178527, 9121899, 
                                                      9098091, 10399161, 12321421, 
                                                      13537762, 13428658, 11360680, 
                                                      11693705, 13937646))


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

impact_sep_fd_lvl3 <- impact_fd |>
  mutate(year = lubridate::year(date),
         week = lubridate::week(date))|>
  group_by(year, week, nuts_id, type_of_class) |>
  tally() |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles,
         ratio_rescale = scales::rescale(ratio)
  ) 

saveRDS(impact_sep_fd_lvl3, "files/impact_by_class_lvl3.RData")
# __Aggreatate into weeks and lvl2 ----------------------------------------

impacts_fd_lvl2 <- impact_fd_lvl3 |>
  ungroup()|>
  mutate(nuts_id = substr(nuts_id, 1, 4))|>
  group_by(year, week, nuts_id) |>
  summarise_at(vars(n), sum) |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles,
         ratio_rescale = scales::rescale(ratio),
         test = paste(year,week, nuts_id, sep = "-")
  ) 




impacts_sep_fd_lvl2 <- impact_sep_fd_lvl3 |>
  ungroup()|>
  mutate(nuts_id = substr(nuts_id, 1, 4))|>
  group_by(year, week, nuts_id, type_of_class) |>
  summarise_at(vars(n), sum) |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles) |>
  select(-c(articles,n)) |>
  pivot_wider(names_from = "type_of_class",
              values_from = "ratio") |>
  mutate(across(energy:livestock, ~ifelse(is.na(.x), 0, .x)),
         ratio = energy+social+agriculture+fire+livestock)


# 2. Test in single NUTS3 -------------------------------------------------

shape_nuts_path <- "./data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp"
shape_nuts <- sf::read_sf(shape_nuts_path) |>
  filter(CNTR_CODE== "DE",
         # LEVL_CODE %in% c(1)
         ) |>
  select(-c(CNTR_CODE,NAME_LATN, MOUNT_TYPE:FID))


impacts_fd_lvl2 |>
  filter(nuts_id == "DE40") |>
  filter(year %in% 2017:2022)|>
  ggplot(aes(x = week, y = ratio, color = factor(year)))+
  geom_path()+
  theme_bw()


  

# 3. Get NUTS raster ------------------------------------------------------

# get shape of all nuts_2
shape_nuts_2 <- shape_nuts|> 
  filter(LEVL_CODE == 2) |>
  select(NUTS_ID, geometry)|>
  magrittr::set_names(c("nuts_id", "geometry"))

# gat impact info into shape
shape_impacts_2 <- full_join(x = shape_nuts_2, 
                  y = impacts_sep_fd_lvl2) |>
  # select(nuts_id,geometry,year, week, ratio) |>
  filter(week <= 52) |>
  mutate(jday = 1+7*(week-1), 
         day = julian_to_date(jday, year)) |>
  select(-c(year, week, jday))

st_write(shape_impacts_2, "files/impact_nuts2_week_multi_impact.shp")

tail(shape_impacts_2)
# __Complete list of shapes by week ---------------------------------------

all_shapes <- expand_grid(nuts_id = unique(shape_impacts_2$nuts_id),
                          year = 2000:2022,
                          week = 1:52) |>
  mutate(jday = 1+7*(week-1), 
         day = julian_to_date(jday, year)) |>
  select(nuts_id, day)

all_impacts_2 <- full_join(x = shape_impacts_2,
                           y = all_shapes, 
                           by = c("nuts_id", "day"))

# View(all_impacts_2)


# pb <- easy_progress_bar(total_it = nrow(all_impacts_2), width_bar = 100)
# for (i in 1:nrow(all_impacts_2)){
#   pb$tick()
# 
#   all_impacts_2$geometry[i] <- shape_nuts_2$geometry[which(shape_nuts_2$nuts_id == all_impacts_2$nuts_id[i])]
# }

all_impacts_2 <- all_impacts_2 |>
  ungroup()|>
  as.data.frame() |>
  select(-geometry) |>
  left_join(shape_nuts_2, by = c("nuts_id")) |>
  mutate(across(energy:ratio, ~ifelse(is.na(.x), 0, .x)))|>
  rename(imp_ratio = ratio) |>
  rename(date = day)

st_write(all_impacts_2, "files/impact_nuts2_week_complete_multi_impact.shp",
         delete_dsn = TRUE)

# get data into list
list_impacts_2 <- split(all_impacts_2, f = all_impacts_2$date)

# 4. Aggregate FD in nuts level -------------------------------------------

fd_brick <- terra::rast("files/germany_fd_pentad.nc") 
fd_brick_impact <- fd_brick[[which(lubridate::year(time(fd_brick)) >= 2000)]]
# plot(fd_brick_impact[[1]])
fd_brick_impact[[1]] |> plot()

shape_fd <- terra::extract(fd_brick_impact, shape_nuts_2, 
                        fun = "mean", na.rm = T, 
                        exact = T)|>
  mutate(nuts_id = shape_nuts_2$NUTS_ID) |>
  select(-ID)

# View(shape_fd)

shape_fd_series <- shape_fd|>
  magrittr::set_names(c(as.character(terra::time(fd_brick_impact)), "nuts_id"))|>
  pivot_longer(cols=1:1606,
               names_to = "date",
               values_to = "fd_ratio")|>
  mutate(date = as.Date(date))

data.table::fwrite(shape_fd_series, "files/fd_series_by_nuts_2.csv")


# 5. Join fd and impact ---------------------------------------------------

"
    - Join fd ratio and impact ratio into single dataframe with complete dates.
    - aggregate into weeks
    - Auto correlation and time series analysis
"

shape_nuts_2 <- shape_nuts |> 
  filter(LEVL_CODE ==2) |> 
  select(NUTS_ID, geometry)


fd_shape <- shape_nuts_2 |> 
  full_join(y = shape_fd_series, by = c("NUTS_ID" = "nuts_id")) |>
  rename("nuts_id" = "NUTS_ID")


full_series <- expand_grid(nuts_id = unique(fd_shape$nuts_id), 
                           date = seq.Date(as.Date("2000-01-01"),
                                           as.Date("2021-12-31"),
                                           by = "day")) |>
  left_join(fd_shape, by = c("nuts_id", "date")) |>
  left_join(all_impacts_2, by = c("nuts_id", "date")) |>
  select(-c(geometry.x,geometry.y))|> 
  tidyr::fill(fd_ratio:imp_ratio, .direction = "down") |>
  mutate(imp_ratio = imp_ratio*1e4) #impact per 10k articles

saveRDS(full_series, file = "files/full_series_lvl2.RData")

# data.table::fwrite(full_series, file = "files/full_series.csv")

full_series |>
  filter(lubridate::year(date) %in% 2002:2004,
         nuts_id == "DE40") |>
  pivot_longer(3:4, names_to = "ratio", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = ratio))+
  geom_path(size = 1)+
  theme_bw()

full_series_bbr <- full_series |>
  filter(nuts_id == "DE40") 

fig <- plot_ly(full_series_bbr, 
               x = ~date, 
               y = ~fd_ratio, 
               name = 'FD prevalence', 
               type = 'scatter', 
               mode = 'lines',
               yaxis = "y1") |>
  add_trace(y = ~imp_ratio, 
            name = 'Impacts', 
            type = 'scatter', 
            mode = 'lines',
            yaxis = "y2") |>
  layout(
    title = "Flash droghts and news media impacts - Brandenburg", 
    yaxis2 = list(overlaying = "y",
                  side = "right",
                  title = "Impact (per 10k articles)",
                  position = 0.95,
                  dtick = 0.03,
                  range = c(0,0.15)
                  ),
    xaxis = list(title="Date",
                 domain = c(0, 0.95)),
    yaxis = list(title="FD prevalence",
                 dtick = 0.1,
                 range = c(0, 0.5)
                 )
  )
  

# REDO IN NUTS1 LEVEL -----------------------------------------------------
# __________________-----------------------------------------------------------
# 6. aggregate impacts to lvl1 ------------------------------------------------

impacts_sep_fd_lvl1 <- impact_sep_fd_lvl3 |>
  ungroup()|>
  mutate(nuts_id = substr(nuts_id, 1, 3))|>
  group_by(year, week, nuts_id, type_of_class) |>
  summarise_at(vars(n), sum) |>
  left_join(number_of_articles_wiso_db) |>
  mutate(ratio = n/articles) |>
  select(-c(articles,n)) |>
  pivot_wider(names_from = "type_of_class",
              values_from = "ratio") |>
  mutate(across(energy:livestock, ~ifelse(is.na(.x), 0, .x)),
         ratio = energy+social+agriculture+fire+livestock)

tally_impact_by_class <- function(impact_by_class)

# get shape of all nuts_1
shape_nuts_1 <- shape_nuts|> 
  filter(LEVL_CODE == 1) |>
  select(NUTS_ID, geometry)|>
  magrittr::set_names(c("nuts_id", "geometry"))

# gat impact info into shape
shape_impacts_1 <- full_join(x = shape_nuts_1, 
                             y = impacts_sep_fd_lvl1) |>
  # select(nuts_id,geometry,year, week, ratio) |>
  filter(week <= 52) |>
  mutate(jday = 1+7*(week-1), 
         day = julian_to_date(jday, year)) |>
  select(-c(year, week, jday))

sf::st_write(shape_impacts_1, "files/impact_nuts1_week_multi_impact.shp")

# __Complete list of shapes by week ---------------------------------------

all_shapes <- expand_grid(nuts_id = unique(shape_impacts_1$nuts_id),
                          year = 2000:2021,
                          week = 1:52) |>
  mutate(jday = 1+7*(week-1), 
         day = julian_to_date(jday, year)) |>
  select(nuts_id, day)

all_impacts_1 <- full_join(x = shape_impacts_1,
                           y = all_shapes, 
                           by = c("nuts_id", "day"))

all_impacts_1 <- all_impacts_1 |>
  ungroup()|>
  as.data.frame() |>
  select(-geometry) |>
  left_join(shape_nuts_1, by = c("nuts_id" = "nuts_id")) |>
  mutate(across(energy:ratio, ~ifelse(is.na(.x), 0, .x)))|>
  rename(imp_ratio = ratio) |>
  rename(date = day)

st_write(all_impacts_1, "files/impact_nuts1_week_complete_multi_impact.shp",
         delete_dsn = TRUE)

# get data into list
list_impacts_1 <- split(all_impacts_1, f = all_impacts_1$date)

# 7. Aggregate FD in nuts level -------------------------------------------

fd_brick <- terra::rast("files/germany_fd_pentad.nc") 
fd_brick_impact <- fd_brick[[which(lubridate::year(terra::time(fd_brick)) >= 2000)]]

# fd_brick_impact[[1]] |> terra::plot()

shape_fd <- terra::extract(fd_brick_impact, shape_nuts_1, 
                           fun = "mean", na.rm = T, 
                           exact = T)|>
  mutate(nuts_id = shape_nuts_1$NUTS_ID) |>
  select(-ID)

# View(shape_fd)

shape_fd_series_lvl1 <- shape_fd |>
  magrittr::set_names(c(as.character(terra::time(fd_brick_impact)), "nuts_id"))|>
  pivot_longer(cols=1:1606,
               names_to = "date",
               values_to = "fd_ratio")|>
  mutate(date = as.Date(date))

data.table::fwrite(shape_fd_series_lvl1, "files/fd_series_by_nuts_1.csv")


# 8. Join fd and impact ---------------------------------------------------

"
    - Join fd ratio and impact ratio into single dataframe with complete dates.
    - aggregate into weeks
    - Auto correlation and time series analysis
"

shape_nuts_1 <- shape_nuts |> 
  filter(LEVL_CODE ==1) |> 
  select(NUTS_ID, geometry)


fd_shape <- shape_nuts_1 |> 
  full_join(y = shape_fd_series_lvl1, by = c("NUTS_ID" = "nuts_id")) |>
  rename("nuts_id" = "NUTS_ID")


full_series <- expand_grid(nuts_id = unique(fd_shape$nuts_id), 
                           date = seq.Date(as.Date("2000-01-01"),
                                           as.Date("2021-12-31"),
                                           by = "day")) |>
  left_join(fd_shape, by = c("nuts_id", "date")) |>
  left_join(all_impacts_1, by = c("nuts_id", "date")) |>
  select(-c(geometry.x,geometry.y))|> 
  tidyr::fill(fd_ratio:imp_ratio, .direction = "down") |>
  mutate(imp_ratio = imp_ratio*1e4) #impact per 10k articles

saveRDS(full_series, file = "files/full_series_lvl1.RData")

# data.table::fwrite(full_series, file = "files/full_series.csv")

full_series |>
  filter(lubridate::year(date) %in% 2002:2004,
         nuts_id == "DE4") |>
  pivot_longer(3:4, names_to = "ratio", values_to = "value") |>
  ggplot(aes(x = date, y = value, color = ratio))+
  geom_path(size = 1)+
  theme_bw()

full_series_bbr <- full_series |>
  filter(nuts_id == "DE4") 

fig <- plot_ly(full_series_bbr, 
               x = ~date, 
               y = ~fd_ratio, 
               name = 'FD prevalence', 
               type = 'scatter', 
               mode = 'lines',
               yaxis = "y1") |>
  add_trace(y = ~imp_ratio, 
            name = 'Impacts', 
            type = 'scatter', 
            mode = 'lines',
            yaxis = "y2") |>
  layout(
    title = "Flash droghts and news media impacts - Brandenburg", 
    yaxis2 = list(overlaying = "y",
                  side = "right",
                  title = "Impact (per 10k articles)",
                  position = 0.95,
                  dtick = 0.03,
                  range = c(0,0.15)
    ),
    xaxis = list(title="Date",
                 domain = c(0, 0.95)),
    yaxis = list(title="FD prevalence",
                 dtick = 0.1,
                 range = c(0, 0.5)
    )
  )


# 9. Get NUTS-1 shape -----------------------------------------------------

shape_nuts_2 <- sf::read_sf("./data/GIS/nuts2.shp")
shape_nuts_1 <- sf::read_sf("./data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |>
  filter(LEVL_CODE == 1,
         CNTR_CODE == "DE") |>
  select(c("NUTS_ID", "LEVL_CODE", "NUTS_NAME", "geometry"))

sf::write_sf(shape_nuts_1, "./data/GIS/nuts1.shp")

saveRDS(shape_nuts_2, "test.RData")

test <- readRDS("test.RData")

min(test == shape_nuts_2)

reticulate::py_save_object(shape_nuts_2, "test1", pickle = "pickle")
test1 <- reticulate::py_load_object("test1", pickle = "pickle")
test1

class(shape_nuts_2)


# __ Get NUTS-3 shape -----------------------------------------------------


shape_nuts_3 <- sf::read_sf("./data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |>
  filter(LEVL_CODE == 3,
         CNTR_CODE == "DE") |>
  select(c("NUTS_ID", "LEVL_CODE", "NUTS_NAME", "geometry"))

sf::write_sf(shape_nuts_3, "./data/GIS/nuts3.shp")
