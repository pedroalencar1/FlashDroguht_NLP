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
img_animated <- magick::image_animate(img_joined, fps = 5)

img_animated

# save to disk
magick::image_write(image = img_animated,
            path = "./figs/fd_2018.gif")

