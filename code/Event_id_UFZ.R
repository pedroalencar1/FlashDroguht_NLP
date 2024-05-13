"
Indentify flash droughts in Germany:
  - Method: Ford and Labosier, 2017
  - Data: UFZ SWC product
  
Pedro Alencar
17.05.2023
"


# 0. Import packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(ncdf4)
library(ncdf4.helpers)
library(terra)
library(tictoc)
library(parallel) 
library(future.apply)
library(sf)
library(progress)
library(magrittr)

library(ggplot2)

source("./code/functions.R")
source("../../@R scripts/Utilities.R")

# 1. Import data as datacube ----------------------------------------------

#list of files
files <- list.files("./data/UFZ_SM/",all.files = T, full.names = T, recursive = T) %>% 
  .[grep("ompr.nc", .)]

# viz. test
swc <- terra::rast(files[1])
# terra::plot(swc[[1]] ,main = "Soil Moisture - 01.1980")

# get layer 1
ras_list_1 <- files %>% purrr::map(~ rast(.x, subds="SWC_L01"))  # Import the raster
ras_stack_1 <- rast(ras_list_1)  # Convert RasterStack to RasterBrick

# get layer 2
ras_list_2 <- files %>% purrr::map(~ rast(.x, subds="SWC_L02"))  # Import the raster
ras_stack_2 <- rast(ras_list_2)  # Convert RasterStack to RasterBrick

# get sum of both layer
ras_stack_full <- ras_stack_1*0.2 + ras_stack_2*0.8

terra::plot(ras_stack_full[[1]])



ncdf4::nc_create("files/swc_l12.nc", )

times <- time(ras_stack_full, format="")
df_aux <- data.frame(i = 1:length(times),
                     times= times,
                     years = lubridate::year(times))

years <- unique(lubridate::year(times))

pb <- easy_progress_bar(length(years))
for (year in years){
  # year = 1980
  pb$tick()
  
  ids <- df_aux$i[which(year == df_aux$years)]
  i_min = min(ids, na.rm = T)
  i_max = max(ids, na.rm = T)
  
  terra::writeCDF(ras_stack_full[[i_min:i_max]], 
                  filename = paste0("files/UFZ_top_soil_combined/swc_l12_", year, ".nc"), 
                  overwrite = T, 
                  varname = "swc_top")
  
}


# 2. summarize by pentad -----------------------------------------------


# get date _beggining_ of each pentad
df <- data.frame(day = time(ras_stack_full), 
                 pentad = get_pentad(time(ras_stack_full)),
                 aux = 1)

for (i in 2:nrow(df)){
  if (df$pentad[i] == df$pentad[i-1]) {
    df$aux[i] <- NA
  }
}

dates <- df |>
  drop_na()|>
  mutate(day = as.Date(day))|>
  select(day) |>
  unlist()|>
  c()|>
  as.Date(origin = as.Date("1970-01-01"))


# group brick into pentads
ufz_brick_pentad <- terra::tapp(ras_stack_full, 
                                get_pentad(time(ras_stack_full)), 
                                mean)

# add dates to pentad brick
terra::time(ufz_brick_pentad) <- dates

terra::writeCDF(ufz_brick_pentad, 
                filename = "files/ufz_germany_brick_pentad.nc", 
                overwrite = T, 
                varname = "swc")


# 3. Identify events ------------------------------------------------------

# ufz_sm_pentad <- terra::rast("files/ufz_germany_brick_pentad.nc") #load data
ufz_fd_pentad <- ufz_brick_pentad # initialize output

pb <- easy_progress_bar(225*175)
for (i in 1:(225*175)){
  pb$tick()
  
  # i = 6250
  if (is.na(ufz_brick_pentad[i][1])) next
  
  input_i <- data.frame(day= dates, sm = t(ufz_brick_pentad[i]), row.names = NULL)
  # if (is.na(input_i$sm[1])) next
  
  fd_i <- raster_FL2017(input_i)
  
  ufz_fd_pentad[i] <- fd_i$fd
}

terra::writeCDF(ufz_fd_pentad, 
                filename = "files/ufz_fd_pentad.nc", 
                overwrite = T, 
                varname = "is_fd")

terra::plot(ufz_fd_pentad[[3100]])

ufz_fd_pentad <- rast("files/ufz_fd_pentad.nc")

plot(ufz_fd_pentad[[1]])

# 4. Get prevalence at each level -----------------------------------------

# get shape into same projection of raster
shape_nuts <- read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |>
  filter(CNTR_CODE == "DE")|>
  st_transform(crs = 31468) #DHDN / Gauß-Krüger Zone 4: https://www.geoportal.rlp.de/mediawiki/index.php/EPSG-Codes/de

# get shapes of each level
shape_nuts_1 <- shape_nuts |>
  filter(LEVL_CODE == 1)

shape_nuts_2 <- shape_nuts |>
  filter(LEVL_CODE == 2)

shape_nuts_3 <- shape_nuts |>
  filter(LEVL_CODE == 3) 
  
# select dates and set raster projection

ufz_fd_pentad_a <- flip(ufz_fd_pentad[[1:1000]])
ufz_fd_pentad_b <- flip(ufz_fd_pentad[[1001:2000]])
ufz_fd_pentad_c <- flip(ufz_fd_pentad[[2001:3139]])

ufz_fd_pentad_all <- c(ufz_fd_pentad_a, ufz_fd_pentad_b, ufz_fd_pentad_c)
ufz_fd_impact <- ufz_fd_pentad_all[[which(lubridate::year(time(ufz_fd_pentad_all)) >= 1980 & 
                                        lubridate::year(time(ufz_fd_pentad_all)) <= 2022)]]
# ufz_fd_impact <- terra::flip(ufz_fd_impact)
crs(ufz_fd_impact) <- crs(shape_nuts)


plot(ufz_fd_impact[[10]], 
     fun=function(){plot(vect(shape_nuts_1), add=TRUE)} 
     )

writeCDF(ufz_fd_impact, "files/ufz_fd_impact_pentad.nc",
         varname = "is_fd",
         unit = "none", 
         overwrite = T)



# function to export data of FD prevalence as dataframe
get_prevalence_fd <- function(spat_raster, shape_nuts, level){
  # 
  # spat_raster = ufz_fd_impact
  # shape_nuts = shape_nuts_1
  # level = 1
  # 
  shape_fd <- terra::extract(spat_raster, shape_nuts, 
                               fun = "mean", na.rm = T, 
                               exact = T) |>
    mutate(nuts_id = shape_nuts$NUTS_ID) |>
    select(-ID)
  
  # View(shape_fd)
  
  number_cols = ncol(shape_fd) - 1
  
  shape_fd_series <- shape_fd |>
    magrittr::set_names(c(as.character(terra::time(spat_raster)), "nuts_id"))|>
    pivot_longer(cols=1:number_cols,
                 names_to = "date",
                 values_to = "fd_ratio")|>
    mutate(date = as.Date(date))
  
  saveRDS(shape_fd_series, 
          file = paste0("files/ufz_fd_series_by_nuts_", level,".RData")
          )
  
  cat(paste0("Data of FD prevalence at NUTS-", level, " saved as RData.\n"))
  
  return(shape_fd_series)
  
}

# 4.1 run function --------------------------------------------------------------

fd_prev_1 <- get_prevalence_fd(ufz_fd_impact, shape_nuts_1, 1)
fd_prev_2 <- get_prevalence_fd(ufz_fd_impact, shape_nuts_2, 2)
fd_prev_3 <- get_prevalence_fd(ufz_fd_impact, shape_nuts_3, 3)


# 5. Get full_series df ---------------------------------------------------

# all articles published yearly
number_of_articles_wiso_db <- data.frame(year = seq(2000,2022,1),
                                         articles = c(3727202, 3805202, 4046952, 
                                                      5279895, 6539592, 7031086, 
                                                      7546534, 7769275, 8381606, 
                                                      8416353, 8652610, 9316843, 
                                                      9447050, 9178527, 9121899, 
                                                      9098091, 10399161, 12321421, 
                                                      13537762, 13428658, 11360680, 
                                                      11693705, 13937646))

impact_fd <-  data.table::fread("data/extracted_impacts_daily_12_12_2022.csv") |>
  select(-V1) |>
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y")))|>
  filter(type_of_class %in% c("fire", 
                              "agriculture", 
                              "livestock", 
                              "energy", 
                              "social")) 

nuts_names <- impact_fd |>
  select(nuts_id, nuts_name) |>
  distinct()

impact_data_2022 <- data.table::fread("data/impacts_daily_2022/pedro_export_2022_b.csv") |>
  select(-V1) |>
  mutate(date = as.Date(date, tryFormats = c("%d.%m.%Y"))) |>
  select(nuts_id, date, type_of_class, id)  |>
  filter(type_of_class %in% c("fire", 
                              "agriculture", 
                              "livestock", 
                              "energy", 
                              "social")) |>
  left_join(nuts_names, by = "nuts_id") |>
  select(nuts_id, nuts_name, date, type_of_class, id)

impact_fd <- rbind(impact_fd, impact_data_2022)


# function to export full series of fd_ratio and impacts

lvl = 2
fd_prev_lvl = fd_prev_2
impact_fd = impact_fd
shape_nuts_lvl = shape_nuts_2

export_full_series <- function(impact_fd, fd_prev_lvl, lvl){
  # # 
  # lvl = 1
  # fd_prev_lvl = fd_prev_1
  # impact_fd = impact_fd
  # shape_nuts_lvl = shape_nuts_1

  shape_nuts_lvl <- read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |>
    filter(CNTR_CODE == "DE", 
           LEVL_CODE == lvl) |>
    select(NUTS_ID, geometry) |>
    rename("nuts_id" = "NUTS_ID")
  
  # get impact by class
  impact_by_class_lvl <- impact_fd |>
    mutate(year = lubridate::year(date),
           week = lubridate::week(date),
           nuts_id = substr(nuts_id, start = 1, stop = 2+lvl))|>
    group_by(year, week, nuts_id, type_of_class) |>
    tally() |>
    ungroup()|>
    left_join(number_of_articles_wiso_db) |>
    mutate(ratio = n/articles,
           ratio_rescale = scales::rescale(ratio)
    ) |>
    pivot_wider(names_from = "type_of_class",
                  values_from = "ratio") |>
    group_by(year, week, nuts_id)|> # group repeated years
    summarise_at(vars(n, energy:livestock), sum, na.rm = T) |>
    mutate(across(n:livestock, ~ifelse(is.na(.x), 0, .x)),
           ratio = energy+social+agriculture+fire+livestock)|>
    ungroup()
  
  # join to shape
  shape_impact_nuts_lvl <- sf::read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |> 
    filter(LEVL_CODE == lvl,
           CNTR_CODE == "DE") |> 
    select(NUTS_ID, geometry) |>
    full_join(impact_by_class_lvl, by = c("NUTS_ID"="nuts_id")) |> #View()
    # select(nuts_id,geometry,year, week, ratio) |>
    filter(week <= 52) |>
    mutate(jday = 1+7*(week-1), 
           day = julian_to_date(jday, year)) |>
    select(-c(year, week, jday))
  
  
  # get complete series
  all_shapes <- expand_grid(NUTS_ID = unique(shape_impact_nuts_lvl$NUTS_ID),
                            year = 2000:2022,
                            week = 1:52) |>
    mutate(jday = 1+7*(week-1), 
           day = julian_to_date(jday, year)) |>
    select(NUTS_ID, day)
  
  all_impacts_lvl <- shape_impact_nuts_lvl |>
    ungroup() |>
    full_join(all_shapes, 
              by = c("NUTS_ID", "day")) |>
    mutate(across(energy:ratio, ~ifelse(is.na(.x), 0, .x)))|>
    rename("nuts_id" = "NUTS_ID",
           "date" = "day",
           "imp_ratio" = "ratio")
  
  # get fd prevalance into shape
  fd_shape_lvl <- shape_nuts_lvl |> 
    full_join(fd_prev_lvl, by = "nuts_id")
  
  
  full_series_lvl <- expand_grid(nuts_id = unique(fd_shape_lvl$nuts_id), 
                             date = seq.Date(as.Date("2000-01-01"),
                                             as.Date("2022-12-31"),
                                             by = "day")) |>
    left_join(fd_shape_lvl, by = c("nuts_id", "date")) |>
    left_join(all_impacts_lvl, by = c("nuts_id", "date")) |>
    select(-c(geometry.x,geometry.y))|> 
    tidyr::fill(fd_ratio:imp_ratio, .direction = "down") |>
    mutate(imp_ratio = imp_ratio*1e4) #impact per 10k articles
  

  # full_series_lvl |>
  #   filter(nuts_id == "DE3") |>
  #   View()


  saveRDS(full_series_lvl, 
          file = paste0("files/ufz_full_series_lvl", lvl,".RData")
          )
  
  cat("Done!")

}

export_full_series(impact_fd, 
                   fd_prev_lvl = fd_prev_1,
                   # shape_nuts_lvl = shape_nuts_1,
                   lvl = 1)

readRDS('files/ufz_full_series_lvl1.RData') |>
  filter(nuts_id == "DE3") |>
  View()

export_full_series(impact_fd, 
                   fd_prev_lvl = fd_prev_2,
                   # shape_nuts_lvl = shape_nuts_2,
                   lvl = 2)

export_full_series(impact_fd, 
                   fd_prev_lvl = fd_prev_3,
                   # shape_nuts_lvl = shape_nuts_3,
                   lvl = 3)



# 6. time series of fd occurence ------------------------------------------

shape_nuts <- read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |>
  filter(CNTR_CODE == "DE")|>
  st_transform(crs = 31468) |>
  filter(LEVL_CODE == 1) %>%
  mutate(area = terra::expanse(terra::vect(.))) |>
  mutate(area = area/1000000) |>
  ungroup() |>
  as.data.frame() |>
  select(NUTS_ID, area)

area_de <- sum(shape_nuts$area) #km2

fd <- readRDS("files/ufz_fd_series_by_nuts_1.RData") |>
  left_join(shape_nuts, by = join_by(nuts_id == NUTS_ID)) |>
  mutate(area_fd = fd_ratio*area) |>
  group_by(date) |>
  summarise_at(vars(area_fd), sum, na.rm = T) |>
  mutate(ratio_fd = area_fd/area_de) |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  summarise_at(vars(ratio_fd), max)

mk_test <- rkt::rkt(fd$year, fd$ratio_fd)

lb1 <- "paste(italic(Slope), \" = 9.11 %.\", decade ^ -1)"
lb2 <- "paste(italic(p-value), \" = 0.001\")"

ggplot(fd, aes(x = year, y = ratio_fd*100))+
  geom_path(color = "#666666", size = 1)+
  geom_smooth(method = "lm", color = "transparent")+
  geom_abline(intercept = -1705,
              slope = mk_test$B*100,
              linetype = "dashed")+
  scale_x_continuous("Year", expand = c(0,0))+
  scale_y_continuous("Max. area affected by flash drought (%)",
                     limits = c(0,100),
                     breaks = seq(0,100,20),
                     expand = c(0,0))+
  annotate("text", x = 2011.7, y = 23, label = lb1, parse=TRUE)+
  annotate("text", x = 2009.5, y = 16, label = lb2, parse=TRUE)+
  theme_bw()


ggsave("figs/trend_area_fd.png",
       units = "cm",
       width = 14, height = 8)


# 7. export FD dataset ----------------------------------------------------

r_fd <- rast("files/ufz_fd_impact_pentad.nc")

bb <- shape_nuts_1 |>
  filter(NUTS_ID %in% c("DE3", "DE4"))

spree <- terra::vect("/Users/alencar/Downloads/Spree-subcatchments.gpkg")
plot(r_fd[[1]])
spree_reproj <- terra::project(spree, r_fd)

spree_fd <- terra::crop(r_fd, spree_reproj)
spree_fd <- terra::mask(spree_fd, spree_reproj)

writeCDF(spree_fd, 
         filename = "files/spree_fd_ford.nc", 
         overwrite = T, 
         varname = "is_fd")


bbr_fd <- terra::crop(r_fd, bb)
bbr_fd <- terra::mask(bbr_fd, bb)

writeCDF(bbr_fd, 
         filename = "files/bbr_fd_ford.nc", 
         overwrite = T, 
         varname = "is_fd")


