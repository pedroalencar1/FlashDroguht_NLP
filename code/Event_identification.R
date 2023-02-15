"
Indentify flash droughts in Germany:
  - Method: Ford and Labosier, 2017
  - Data: Gleam 3.6a soil moisture
  
Pedro Alencar
01.02.2023
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


# 1. Import data as datacube ----------------------------------------------

#list of files
files <- list.files("./data/nc_gleam",all.files = T, full.names = T, recursive = T) %>% 
  .[grep("v3.6a.nc", .)]

# vis. test  
sm <- terra::rast(files[1])
# terra::plot(sm[[1]] ,main = "Soil Moisture - 01.1980")

# create raster stack
ras_list = files %>%  purrr::map(~ rast(.x))  # Import the raster
ras_stack = rast(ras_list)  # Convert RasterStack to RasterBrick

inMemory(ras_stack[[1]]) # check if loaded

dim(ras_stack) #[x: y: z]- 15341 raster layers (days) with 720 x 1440 cells

# plot(ras_stack[[15341]])

# __1.1 example: extract ts of single pont --------------------------------

Long=13
Lat=52
# Creating a spatial vector object from the location coords
app_berlin = vect(sp::SpatialPoints(cbind(Long, Lat)))

# Extract time series for the location
sm_berlin=terra::extract(ras_stack,
                        app_berlin,    # lat-long as spatial locations
                        method='simple')  # or "simple" for nearest neighbor

# Create a dataframe using the dates (derived from raster layer names) and extracted values
sm_berlin_ts=data.frame(Time=c(1:nlyr(ras_stack)), # Sequence of retrieval time
                   SM=as.numeric(sm_berlin[,-1]))     #NDVI values
# Try changing Time to as.Date(substr(colnames(ndvi_val),13,22), format = "%Y.%m.%d")

# Plot NDVI time series extracted from raster brick/stack
plot(sm_berlin_ts, type="l", col="maroon", ylim=c(0,0.5))


# __1.2 extract by mask (germany) ----------------------------------------

# load shape germany
shape_path <- "data/GIS/world/TM_WORLD_BORDERS-0.3.shp"

shape_germany <- sf::read_sf(shape_path) |>
  filter(iso3 == "DEU")
# terra::plot(shape_germany)  

# years <- seq(1980,2021, 1)

# crop to germany size
tic()
ras_stack_crop <- terra::crop(ras_stack,shape_germany)
toc()

# mask to germany shape
tic()
de_brick <- terra::mask(ras_stack_crop, shape_germany)
toc()

terra::writeCDF(de_brick, 
                filename = "files/gleam_germany_brick.nc", 
                overwrite = T, 
                varname = "SMroot")

terra::plot(de_brick[[1]] ,main = time(de_brick),
            fun=function(){plot(vect(shape_germany), add=TRUE)},
            ext=c(6, 16, 46.5, 55.5))


# __1.3 summarize by pentad -----------------------------------------------

# function to get pentad number. Leap year agregated in February
get_pentad <- function(time){
  julian <- lubridate::yday(time)
  leap <- lubridate::leap_year(time)
  year <-  lubridate::year(time)
  
  pentad <- ceiling((julian + leap*((julian >= 60)*-1))/5) 
  pentad <- pentad + (year-1980)*73
  
  return(pentad)
}

# get date _beggining_ of each pentad
df <- data.frame(day = time(de_brick), 
                 pentad = get_pentad(time(de_brick)),
                 aux = 1)

for (i in 2:nrow(df)){
  if (df$pentad[i] == df$pentad[i-1]) {
    df$aux[i] <- NA
  }
}

dates <- df |>
  drop_na()|>
  select(day) |>
  unlist()|>
  c()|>
  as.Date(origin = as.Date("1970-01-01"))

# group brick into pentads
de_brick_pentad <- terra::tapp(de_brick,  get_pentad(time(de_brick)), mean)

# add dates to pentad brick
terra::time(de_brick_pentad) <- dates

terra::writeCDF(de_brick_pentad, 
                filename = "files/gleam_germany_brick_pentad.nc", 
                overwrite = T, 
                varname = "SMroot")


# 2. Identify events in each pixel ----------------------------------------

de_sm_pentad <- terra::rast("files/gleam_germany_brick_pentad.nc") #load data
de_fd_pentad <- de_sm_pentad # initialize output



#' Ford and Labosier method for raster brick
#'
#' @param swc data frame with dates (as pentads) and sm (mean in the pentad)
#' @param crit 
#'
#' @return
#' 
#' 
#'
#' @examples
raster_FL2017 <- function(swc, crit = c(40, 20, 30)){
  # crit = c(40, 20, 30)
  # swc = df
  
  crit1 = crit[1]
  crit2 = crit[2]
  crit3 = crit[3]
  
  colnames(swc) <- c("time", "swc")
  
  #get series and matrix of soil moisture
  series.swc <- swc
  pentad.swc <- matrix(swc$swc, nrow = 73, byrow = F)
  
  #get series and matrix of percentiles
  percentile.swc <- t(apply(pentad.swc, 1, fdClassify::f.percentile))
  percentile.series <- c(percentile.swc)
  
  # remove NA, save positions
  firstNonNA <- min(which(!is.na(percentile.series)))
  lastNonNA <- max(which(!is.na(percentile.series)))
  percentile.series <- percentile.series[firstNonNA:lastNonNA]
  
  # get possible intensification intervals
  a1 <- diff(percentile.series, lag = 1) %>% c(rep(100, 1), 
                                               .)
  a2 <- diff(percentile.series, lag = 2) %>% c(rep(100, 2), 
                                               .)
  a3 <- diff(percentile.series, lag = 3) %>% c(rep(100, 3), 
                                               .)
  a4 <- diff(percentile.series, lag = 4) %>% c(rep(100, 4), 
                                               .)
  data.table <- as.data.frame(cbind(percentile.series, a1, 
                                    a2, a3, a4))
  # get highest drop
  data.table$a.min <- sapply(1:nrow(data.table), 
                             function(i) min(data.table[i,2:5], na.rm = T))

  # get min intensification period
  data.table$p.min <- apply(data.table[,2:5], 1, which.min)
  
  # get fd onset date
  data.table$fd <- 0
  for (i in 2:(nrow(data.table) - 4)) {
    data.table$fd[i] <- (data.table$percentile.series[i] <= crit2) * 
                        (data.table$percentile.series[i - data.table$p.min[i]] >= crit1) * 
                        (max(data.table$percentile.series[(i + 1):(i + 3)]) <= crit3)
    
  }

  data.table$p.min <- data.table$p.min * data.table$fd
  data.table[is.na(data.table)] <- 0
  
  # get complete duration of fd
  data.table$fd2 <- 0
  for (i in 2:nrow(data.table)){
    if (data.table$fd[i] == 1) {
      # i = 3028
      before <- i-data.table$p.min[i]
      after <- min(i + min(which(data.table$percentile.series[(i+1):nrow(data.table)] > crit1), nrow(data.table)) - 1,
                   nrow(data.table))
      
      data.table$fd2[before:after] <- 1
    }
  }
  
  series.swc$fd <- data.table$fd2
  
  return(series.swc)
}


tic()
pb <- utils::txtProgressBar(min = 0, max = (31*37), style = 3, width = 80)
k <- 0
for (i in 1:(31*37)){
  k<-k+1
  utils::setTxtProgressBar(pb, k)
  
  # i = 1
  input_i <- data.frame(day= dates, sm = t(de_sm_pentad[i]), row.names = NULL)
  
  if (is.na(input_i$sm[1])) next
  
  fd_i <- raster_FL2017(input_i)

  de_fd_pentad[i] <- fd_i$fd
}
toc()

terra::writeCDF(de_fd_pentad, 
                filename = "files/germany_fd_pentad.nc", 
                overwrite = T, 
                varname = "is_fd")


# 3. Plotting -------------------------------------------------------------

shape_nuts_path <- "data/GIS/kreise_bld/vg2500_krs.shp"
shape_nuts <- sf::read_sf(shape_nuts_path) 

i <- which(time(de_fd_pentad) == "2018-07-05")
terra::plot(de_fd_pentad[[i]] ,main = time(de_fd_pentad)[i],
            fun=function(){plot(vect(shape_nuts), add=TRUE, border = "darkgrey")},
            ext=c(6, 16, 46.5, 55.5),
            col=c("white", "red"))

for (id in ids){
  # id =2775
  png(file=paste0("./figs/maps_2018/",id, ".png"))
  terra::plot(de_fd_pentad[[id]] ,main = time(de_fd_pentad)[id],
              fun=function(){plot(vect(shape_nuts), add=TRUE, border = "darkgrey")},
              ext=c(6, 16, 46.5, 55.5),
              col=c("white", "red"))
  dev.off()
}

imgs <- list.files("./figs/maps_2018", full.names = TRUE)
img_list <- lapply(imgs, magick::image_read)
img_joined <- magick::image_join(img_list)
img_animated <- magick::image_animate(img_joined, fps = 2)

img_animated

# save to disk
magick::image_write(image = img_animated,
            path = "./figs/fd_2018.gif")

