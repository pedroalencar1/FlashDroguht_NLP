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

