"
Study the delay between increase of FD affected area and media report


Pedro Alencar
21.03.2023
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
library(fdClassify)

source("../../@R scripts/Utilities.R")

shape_nuts_2 <- sf::read_sf("./data/GIS/nuts2.shp")
df <- readRDS("files/full_series.RData")

units <- unique(df$nuts_id)

# 1. Get percentiles of ratio and changes -------------------

#' get percentiles from series
perc_from_series <- function(year, id, val){
  # year = df_unit$year
  # id = df_unit$week
  # val = df_unit$fd_diff_1
  
  id_val <- data.frame(year = year, 
                       id = id,
                       val = val) |> 
    # mutate(id = as.factor(id))|>
    ungroup()|>
    pivot_wider(names_from = "id",
                values_from = "val") |> 
    select(-year) |>
    sapply(f.percentile) |>
    t() |>
    c()
  
  return(id_val)
}

# get all relevant percentiles from a unit
get_percentiles <- function(df, unit) {
  
  df_unit <- df |>
    filter(nuts_id == unit) |>
    mutate(year = lubridate::year(date),
           week = lubridate::week(date),
           week = ifelse(week == 53, 52, week)) |>
    group_by(year, week) |>
    summarise_at(vars(fd_ratio, imp_ratio),
                 .funs =  c("sum" = sum, "mean" = mean)) |>
    select(-c(fd_ratio_sum, imp_ratio_mean)) |>
    rename(c(fd_ratio = fd_ratio_mean,
             imp_ratio = imp_ratio_sum)) |>
    ungroup()|>
    mutate(fd_diff_1 = c(0, diff(fd_ratio)),
           fd_diff_2 = c(0,0, diff(fd_ratio, lag = 2)),
           fd_diff_3 = c(0,0,0, diff(fd_ratio, lag = 3)),
           fd_diff_1 = ifelse(fd_diff_1 < 0, 0, fd_diff_1),
           fd_diff_2 = ifelse(fd_diff_2 < 0, 0, fd_diff_2),
           fd_diff_3 = ifelse(fd_diff_3 < 0, 0, fd_diff_3)) |>
    mutate(imp_perc = perc_from_series(year = year,
                                      id = week, 
                                      val = imp_ratio),
           fd_perc = perc_from_series(year = year,
                                      id = week, 
                                      val = fd_ratio),
           fd_diff_1_perc = perc_from_series(year = year,
                                             id = week, 
                                             val = fd_diff_1),
           fd_diff_2_perc = perc_from_series(year = year,
                                             id = week, 
                                             val = fd_diff_2),
           fd_diff_3_perc = perc_from_series(year = year,
                                             id = week, 
                                             val = fd_diff_3))|>
    mutate(imp_perc = ifelse(imp_ratio == 0, 0, imp_perc),
           fd_perc = ifelse(fd_ratio == 0, 0, fd_perc),
           fd_diff_1_perc = ifelse(fd_diff_1 == 0, 0, fd_diff_1_perc),
           fd_diff_2_perc = ifelse(fd_diff_2 == 0, 0, fd_diff_2_perc),
           fd_diff_3_perc = ifelse(fd_diff_3 == 0, 0, fd_diff_3_perc),
           nuts_id = unit)

  return(df_unit)
}

df_perc <- data.frame()
pb <- easy_progress_bar(length(units))
for (unit_id in units){
  pb$tick()
  df_unit <- get_percentiles(df = df, 
                             unit = unit_id)
  
  df_perc <- rbind(df_perc, df_unit)
  
}

saveRDS(df_perc, 
        file = "files/full_series_perc.RData")

# 1.1. Example plot -------------------------------------------------------


all_lags <- expand_grid(unit = units,
                        year = 2000:2021, 
                        lag = NA, 
                        ccf = NA)
pb <- easy_progress_bar(22*38)
for (i in 1:(22*38)){
  lag_max = 20
  
  # i = 30
  pb$tick()
  
  df_aux <- df_perc |> 
    ungroup()|>
    filter(year == all_lags$year[i],
           nuts_id == all_lags$unit[i])  |>
    mutate(imp_perc = ifelse(imp_perc > 80, 1, 0), # identify pulses
           fd_diff_1_perc = ifelse(fd_diff_1_perc > 80, 1, 0))
  
  val1 <- try(ccf(x = df_aux$imp_perc, 
                 y = df_aux$fd_diff_1_perc,
                 lag.max = lag_max,
                 "correlation", 
                 plot = F)$acf %>%
                # .[(lag_max+1):(lag_max+11)] %>%
               which.max() %>%
               ifelse(length(.) == 0, 0, .)
             )
  
  val2 <- try(ccf(x = df_aux$imp_perc, 
                  y = df_aux$fd_diff_1_perc,
                  lag.max = lag_max,
                  "correlation", 
                  plot = F)$acf %>%
                # .[(lag_max+1):(lag_max+11)] %>%
                max() %>%
                ifelse(length(.) == 0, 0, .)
  )
  
  if(inherits(val, "try-error")){
    next
  } else {
    all_lags$lag[i] <- (val1)
    all_lags$ccf[i] <- (val2)
  }
}
# View(all_lags)

all_lags |>
  filter(lag > 0) |>
  mutate(lag = lag - lag_max) |>
  ggplot(aes(x = lag-1))+
  geom_histogram(aes(y = after_stat(density)),bins = (2*lag_max+1)) +
  geom_density(adjust = 1/2.5)+
  geom_hline(yintercept = 0.015, 
             color = "blue",
             linetype="dotdash",
             linewidth = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.09), breaks = seq(0,0.09, 0.03))+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Delay (weeks)",
       y = "Density",
       title = "Distribution of delay between FD onset and impact perception",
       caption = "Delay measured in weeks. \nNegative delays indicate baseline concern on droughts")+
  theme_bw()

all_lags |>
  filter(unit == "DE40")|>
  filter(lag > 0) |>
  mutate(lag = lag - lag_max) |>
  ggplot(aes(x = lag-1))+
  geom_histogram(aes(y = after_stat(density)),bins = (lag_max+1)) +
  geom_density(adjust = 1/2)+
  geom_hline(yintercept = 0.015, 
             color = "blue",
             linetype="dotdash",
             linewidth = 1)+
  scale_y_continuous(expand = c(0,0), limits = c(0, 0.09), breaks = seq(0,0.09, 0.03))+
  scale_x_continuous(expand = c(0,0))+
  labs(x = "Delay (weeks)",
       y = "Density",
       title = "Distribution of delay between FD onset and impact perception",
       caption = "Delay measured in weeks. \nNegative delays indicate baseline concern on droughts")+
  theme_bw()


  
all_lags |>
  filter(lag > 0) |>
  mutate(lag = lag - lag_max) |>
  filter(lag %in% 3:4) |>
  drop_na() |>
  ggplot(aes(x = ccf))+
  geom_histogram(aes(y = after_stat(density))) +
  geom_density(adjust = 0.75)+
  scale_x_continuous(expand = c(0,0), limits = c(0,1))+
  theme_bw()
  


  
  

max_diff <- df_perc |>
  group_by(year, nuts_id) |>
  summarise_at(vars(fd_diff_1_perc), max) |>
  rename(c(unit = "nuts_id"))

all_lags |>
  mutate(lag = lag - 11) |>
  left_join(max_diff, by = c("year", "unit")) |> View()


# use rolling kernel instead of yearly


test <- df_perc_increase
test[test == 0] <- NA

test1 <- filter(test, nuts_id == "DE40",
                year == 2018) |>
   mutate(imp_perc = ifelse(imp_perc > 80, 1, 0),
          fd_diff_1_perc = ifelse(fd_diff_1_perc > 80, 1, 0))
test2 <- filter(df_perc_increase, nuts_id == "DE40", 
                year == 2018)|>
  mutate(imp_perc = ifelse(imp_perc > 80, 1, 0),
         fd_diff_1_perc = ifelse(fd_diff_1_perc > 80, 1, 0))

ccf(x = test1$imp_perc, 
    y = test1$fd_diff_1_perc,
    lag.max = 10,
    na.action = na.pass
    # na.action = na.contiguous
    )

ccf(x = test2$imp_perc,
    y = test2$fd_diff_1_perc,
    lag.max = 10)


ggplot(test2)+
  geom_path(aes(x = week, y = imp_perc))+
  geom_path(aes(x = week, y = fd_diff_1_perc), color = "red")

TSA::prewhiten(x = test2$imp_perc, 
               y = test2$fd_diff_1)
