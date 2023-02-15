
library(terra)

shape_nuts <- sf::read_sf("files/impact_nuts2_week.shp")
raster_drought <- terra::rast("files/germany_fd_pentad.nc")

# plot shapefile => macroregions
shape_nuts |>
  select(nuts_id, geometry)|>
  plot()

# plot raster => single day
plot(raster_drought[[195]])


"
OBS: nota que no shape file cada poligono tem varios valores (datas). 
     Acho que o ideal é primeiro criar uma lista com varios shapes, um para 
     cada data pra só então rasterizar. Ai vou ter uma lista com varios rasters
     que posso transformat em um brick.
     
     Só não sei ao certo como fazer ahahah
"
test <- terra::rasterize(x = shape_nuts, y = raster_drought, field = "ratio")
test
