"
This script is to analyse the data on impact reporting on google trends

25.10.2023
"

# 0. import libraries -----------------------------------------------------


library(dplyr)
library(tidyr)
library(magrittr)
library(terra)
library(tictoc)
library(ggplot2)
library(terra)
library(plotly)
library(fdClassify)
library(hash)

source("../../@R scripts/Utilities.R")
shape_nuts_lvl <- sf::read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |> 
  filter(CNTR_CODE == "DE",
         LEVL_CODE == 1)


# 1. get data -------------------------------------------------------------

# get paths
path_to_files <- "data/google trends drought"
files <- list.files(path_to_files, full.names = T)

# map state codes and names
states <- hash()
states[["BE"]] <- "Berlin"
states[["BB"]] <- "Brandenburg"
states[["BW"]] <- "Baden-Württemberg"
states[["BY"]] <- "Bayern"
states[["HE"]] <- "Hessen"
states[["MV"]] <- "Mecklenburg-Vorpommern"
states[["NI"]] <- "Niedersachsen"
states[["NR"]] <- "Nordrhein-Westfalen"
states[["RP"]] <- "Rheinland-Pfalz"
states[["SL"]] <- "Saarland"
states[["SN"]] <- "Sachsen"
states[["TH"]] <- "Thüringen"
states[["BR"]] <- "Bremen" #missing
states[["HA"]] <- "Hamburg"#missing
states[["SH"]] <- "Schleswig-Holstein"#missing
states[["SA"]] <- "Sachsen-Anhalt"#missing

# set df
df_files <- data.frame(path = files)|>
  mutate(state_code = substr(path, 28, 29))

get_name <- function(x) states[[toupper(x)]]
df_files$state_name <- lapply(df_files$state_code, get_name) |>
  unlist()
  
df_files <- left_join(df_files, 
                      select(shape_nuts_lvl, c(NUTS_ID, NUTS_NAME)), 
                      by = join_by(state_name == NUTS_NAME))


# 2. Get new complete df --------------------------------------------------

df <- read.csv(df_files$path[1])
colnames(df)
df |>
  mutate(date = as.Date(date))|>
  # filter(lubridate::year(date) == 2008)|>
  ggplot(aes(x = date, y = Dürre))+
  geom_path()

df <- read.csv(df_files$path[1]) |>
  mutate(year = lubridate::year(date),
       week = lubridate::week(date))|>
  group_by(year, week) |>
 summarise_at(vars(Dürre), mean) |>
  mutate(index = Dürre/100) |>
  select(-Dürre) 

