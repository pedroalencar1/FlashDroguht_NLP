"
This script is to analyse the data on impact reporting on google trends

25.10.2023
"

# 0. import libraries -----------------------------------------------------


library(dplyr)
library(tidyr)
library(data.table)
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

plot(terra::vect(shape_nuts_lvl))

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

#ref: https://serpapi.com/google-trends-locations
geos_nuts <- hash()
geos_nuts[['DE-BW']] <- 'DE1'
geos_nuts[['DE-BY']] <- 'DE2'
geos_nuts[['DE-BE']] <- 'DE3'
geos_nuts[['DE-BB']] <- 'DE4'
geos_nuts[['DE-HB']] <- 'DE5'
geos_nuts[['DE-HH']] <- 'DE6'
geos_nuts[['DE-HE']] <- 'DE7'
geos_nuts[['DE-MV']] <- 'DE8'
geos_nuts[['DE-NI']] <- 'DE9'
geos_nuts[['DE-NW']] <- 'DEA'
geos_nuts[['DE-RP']] <- 'DEB'
geos_nuts[['DE-SL']] <- 'DEC'
geos_nuts[['DE-SN']] <- 'DED'
geos_nuts[['DE-ST']] <- 'DEE'
geos_nuts[['DE-SH']] <- 'DEF'
geos_nuts[['DE-TH']] <- 'DEG'
  
# __ get data from missing states -----------------------------------------

path_to_missing <- list.dirs("code/python/gt_data", recursive = F)

states_missing <- c("BR", "HA", "SH", "SA")

for (i in 1:length(path_to_missing)){

  files_missing <- list.files(path_to_missing[i], full.names = T)
  
  df <- rbindlist(lapply(files_missing, fread)) |>
    mutate(date = as.Date(date)) %>%
    .[order(.$date),]
  
  file_name <- paste0("data/google trends drought/",
                      tolower(states_missing[i]),
                      "_drought.csv")
  
  write.csv(df, file_name)
}

files_2022 <- list.files("code/python/missing_data_2022", full.names = T)

df_2022 <- data.frame()
for (file in files_2022){
  # file = files_2022[1]
  id <- stringr::str_match(file, "gt_\\s*(.*?)\\s*_2022_1")[2]
  
  df <- fread(file) |>
    mutate(nuts_id = id)
  
  df_2022 <- rbind(df_2022, df)
  
}


# _get complete list of files ---------------------------------------------

# set df
df_files <- data.frame(path = files)|>
  mutate(state_code = substr(path, 28, 29))

get_name <- function(x) states[[toupper(x)]]
df_files$state_name <- lapply(df_files$state_code, get_name) |>
  unlist()

df_files <- left_join(df_files, 
                      select(shape_nuts_lvl, c(NUTS_ID, NUTS_NAME)), 
                      by = join_by(state_name == NUTS_NAME))

plot(df_files$geometry, col = "lightblue")

# 2. Get new complete df --------------------------------------------------

df_complete <- data.frame()
for (i in 1:nrow(df_files)){
  # i =3
  
  df <- read.csv(df_files$path[i]) |> 
    mutate(nuts_id = df_files$NUTS_ID[i],
           date = as.Date(date)) |>
    select(date, Dürre_unscaled, Dürre_monthly, 
           isPartial, scale, Dürre, nuts_id)
  
  df_complete <- rbind(df_complete, df)
}


df_2022_join <- df_2022 |>
  select(date, nuts_id, Dürre) |>
  mutate(date = as.Date(date))

df_2022_join

for (i in 1:nrow(df_2022_join)){
  df_2022_join$nuts_id[i] <- geos_nuts[[df_2022_join$nuts_id[i]]]
}


df_complete_gt <- full_join(df_complete, df_2022_join, 
                            by = c("date", "nuts_id")) |>
  mutate(Dürre = ifelse(is.na(Dürre.x), Dürre.y, Dürre.x)) |>
  select(-c(Dürre.x, Dürre.y)) |>
  mutate(year = lubridate::year(date),
         week = lubridate::week(date),
         week = ifelse(week == 53, 52, week))|>
  group_by(year, week, nuts_id) |>
  summarise_at(vars(Dürre), mean) |>
  mutate(index = Dürre/100) |>
  select(-Dürre) |>
  ungroup()


saveRDS(df_complete_gt, "files/google_trends/complete_series.RData")



# __get complete series for app -------------------------------------------

gt_series <- expand_grid(date = seq(as.Date("2004-01-01"), as.Date("2022-12-31"), by = "day"),
                         nuts_id = unique(df_complete_gt$nuts_id)) |>
  mutate(year = lubridate::year(date),
         week = lubridate::week(date),
         week = ifelse(week == 53, 52, week)) |>
  left_join(df_complete_gt, by = c("nuts_id", "week", "year"))

#' DE2 - 2006 w36-39 missing (BY)
#' DE5 missing (HB)


# ____data HB -------------------------------------------------------------
files_hb <- list.files("/Users/alencar/Library/CloudStorage/OneDrive-Personal/@_PostDoc/NLP_FlashDrought/NLP_r/code/python/gt_data/DE-HB",
                       full.names = T)

df_hb <- data.frame()
for (file in files_hb) {
  aux <- read.csv(file)
  
  df_hb <- rbind(df_hb,aux)
}

df_hb <- df_hb |>
  select(-c(Dürre_unscaled:scale)) |>
  mutate(year = lubridate::year(date),
         week = lubridate::week(date),
         week = ifelse(week == 53, 52, week)) |>
  group_by(year, week) |>
  summarise_at(vars(Dürre), mean) |>
  mutate(index = Dürre/100) |>
  select(-Dürre) |>
  ungroup() |>
  mutate(nuts_id = "DE5")


# ____ data BY ------------------------------------------------------------

#' not available


# ____merge datasets ------------------------------------------------------

gt_series_lvl1 <- left_join(gt_series, df_hb, by = c("year", "week", "nuts_id")) |>
  mutate(index = ifelse(is.na(index.x), index.y, index.x),
         index = ifelse(is.na(index), 0, index)) |>
  select(-c(index.x, index.y)) 

fd_data_1 <- readRDS("/Users/alencar/Library/CloudStorage/OneDrive-Personal/@_PostDoc/NLP_FlashDrought/NLP_r/files/ufz_full_series_lvl1.RData")|>
  filter(lubridate::year(date) >= 2004) |>
  select(date, nuts_id, fd_ratio) 

gt_series_lvl1 <- left_join(gt_series_lvl1, fd_data_1, by = c("date", "nuts_id")) |>
  select(-c(year, week))

colnames(gt_series_lvl1) <- c("date", "nuts_id", "imp_ratio", "fd_ratio")

saveRDS(gt_series_lvl1, "files/gt_full_series_lvl1.RData")

# 3. delay analysis -------------------------------------------------------
df_lvl1 <- readRDS(file = "files/ufz_full_series_lvl1.RData") |>
  select(nuts_id, date, fd_ratio) |>
  mutate(year = lubridate::year(date),
         week = lubridate::week(date),
         week = ifelse(week == 53, 52, week)) |>
  filter(year >= 2004) |>
  group_by(nuts_id, year, week) |>
  summarise_at(vars(fd_ratio), mean)


df_imp_d <- full_join(df_lvl1, df_complete_gt, by = c("nuts_id", "year", "week")) |>
  setnames("index","imp_ratio") |>
  filter(year <= 2022) |>
  ungroup() |>
  mutate(imp_ratio = ifelse(is.na(imp_ratio), 0, imp_ratio)) #fill missing month in DE2


saveRDS(df_imp_d, "files/google_trends/complete_impact_drought.RData")


# __ define a few dedicated functions -------------------------------------

# get all relevant percentiles from a unit
get_percentiles_gt <- function(df, unit, thres = 0.8) {
  # df = df_imp_d
  # unit = units_l1[9]
  
  df_unit <- df |>
    filter(nuts_id == unit) |>
    ungroup()|>
    mutate(fd_diff_1 = c(0, diff(fd_ratio)),
           fd_diff_2 = c(0,0, diff(fd_ratio, lag = 2)),
           fd_diff_3 = c(0,0,0, diff(fd_ratio, lag = 3)),
           fd_diff_1 = ifelse(fd_diff_1 < 0, 0, fd_diff_1),
           fd_diff_2 = ifelse(fd_diff_2 < 0, 0, fd_diff_2),
           fd_diff_3 = ifelse(fd_diff_3 < 0, 0, fd_diff_3),
           fd_diff_1_20p = stats::quantile(fd_diff_1, probs = thres),
           fd_diff_2_20p = stats::quantile(fd_diff_2, probs = thres),
           fd_diff_3_20p = stats::quantile(fd_diff_3, probs = thres),
           fd_diff_1 = fd_diff_1 - fd_diff_1_20p,
           fd_diff_2 = fd_diff_2 - fd_diff_2_20p,
           fd_diff_3 = fd_diff_3 - fd_diff_3_20p,
           fd_diff_1 = ifelse(fd_diff_1 < 0, 0, fd_diff_1),
           fd_diff_2 = ifelse(fd_diff_2 < 0, 0, fd_diff_2),
           fd_diff_3 = ifelse(fd_diff_3 < 0, 0, fd_diff_3),
           ) |>
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

get_perc_lvl_gt <- function(df_lvl, units_lvl, thres = 0.8){
  df_lvl = df_imp_d
  df_perc <- data.frame()
  pb <- easy_progress_bar(length(units_lvl))
  for (unit_id in units_lvl){
    pb$tick()
    # unit_id = units_l1[9]
    df_unit <- get_percentiles_gt(df = df_lvl, 
                               unit = unit_id, 
                               thres = thres)
    
    df_perc <- rbind(df_perc, df_unit)
  }
  
  return(df_perc)
}

get_all_lags <- function(df_perc_lvl, units_lvl, lag_max = 15){
  
  # df_perc_lvl <- df_perc_lvl1
  # units_lvl <- units_l1
  
  n_units <- length(units_lvl)
  n_years <- length(unique(df_perc_lvl$year)) 
  
  all_lags <- expand_grid(unit = units_lvl,
                          year = unique(df_perc_lvl$year), 
                          lag = NA, 
                          ccf = NA)
  
  pb <- easy_progress_bar(n_years*n_units)
  
  for (i in 1:(n_years*n_units)){
    
    pb$tick()
    
    df_aux <- df_perc_lvl |> 
      ungroup()|>
      filter(year == all_lags$year[i],
             nuts_id == all_lags$unit[i])  |>
      mutate(imp_perc = ifelse(imp_perc > 95, 1, 0), # identify pulses
             fd_diff_1_perc = ifelse(fd_diff_1_perc > 95, 1, 0))
    
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
    
    all_lags$lag[i] <- (val1)
    all_lags$ccf[i] <- (val2)
    
  }
  
  return(all_lags)
}

# __ run functions --------------------------------------------------------


perc_df_gt <- get_perc_lvl_gt(df_imp_d, units_l1, thres = 0.8)

lags_df_gt <- get_all_lags(perc_df_gt, units_l1,lag_max = 15)

select_lag_gt <-lags_df_gt |>
  filter(lag > 15 & lag <= 18) 

sqrt(mean(select_lag$ccf, na.rm =T))

test <- lags_df_gt |>
  filter(!is.na(ccf)) |>
  mutate(lag1 = lag-16) |>
  # filter(lag1==+3)
  filter(ccf > 0)
test

plot_delay(test, nuts = 1, lag_max = 15)

#' the analysis shows that google trends has a lower sensibility to milder
#' or smaller flash droughts than on the media.
#' 
#' Looking into the data itseld and the baseline on the plot, there is an overall
#' more uniform interst on the topic when compared to news articles, which are
#' likely influenced by the journalistic interest of specialized media, while 
#' private citizens would be interested on such droughts only in more extreme 
#' cases. 
#' 
#' Nevertheless, the delay between onset and peak of interest is shorter than on 
#' the news by 1 to 2 weeks. Also, there is a second peak after 10 weeks that could
#' be spurious correlation or a rekindled interest after news publications or for
#' long-persistance droughts, althoguh we could not confirm this fact on our 
#' research.


# __ new plots ------------------------------------------------------------

#' generate density/histogram plots with only positive values of delay.

#' function to plot kernel distribution and histogram of delays
plot_delay_new <- function(all_lags, lag_max = 15, nuts = 1,
                           .adjust = 1){
  # all_lags = test
  all_lags = all_lags_list[[3]]
  colors <- c("Histogram" = "#999999", 
              "Empiric PDF" = "#000000", 
              "Baseline" = "blue")
  
  
  all_lags_aux <- all_lags |>
    filter(lag > 0) |>
    mutate(lag = lag - lag_max - 1) 
  
  min(all_lags_aux$lag)
  
  # get intercept of baseline
  stat <- density(all_lags_aux$lag)
  df_stat <- data.frame(x = stat$x,
                        y = stat$y) |>
    filter(x < 0) |>
    filter(x > -1*lag_max)
  
  base_line_y <- mean(df_stat$y)
  
  # get plot
  lag_plot <- all_lags_aux |>
    filter(lag >= 0) |> 
    ggplot(aes(x = lag))+
    geom_histogram(aes(y = after_stat(density),
                       fill = "Histogram"),
                   bins = (lag_max+1),
                   color = "#888888",
                   alpha = 0.8) +
    # geom_vline(aes(xintercept = 0.3, color = "Baseline"),
    #            linetype="dash",
    #            linewidth = 1)+
    geom_density(aes(color = "Empiric PDF"),
                 adjust = 1,
                 linewidth = 0.5)+
    geom_hline(aes(color = "Baseline",
                   yintercept = base_line_y), 
               linetype="dotdash",
               linewidth = 1)+
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, 0.12),
                       breaks = seq(0,0.2, 0.02))+
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(0,lag_max,2))+
    scale_color_manual("",values = colors)+
    scale_fill_manual("", values=colors)+
    labs(x = "Delay (weeks)",
         y = "Density",
         # title = "Distribution of delay between FD onset and impact perception",
         # subtitle = paste0("Data aggregated into NUTS-", nuts),
         # caption = "Delay measured in weeks.
         #          Negative delays indicate baseline concern on droughts (blue line).",
         # tag = ""
         )+
    theme_bw()+
    theme(legend.position = 'none', 
          legend.spacing.x = unit(0.2, 'cm'),
          legend.spacing.y = unit(-0.2, 'cm'),
          legend.text = element_text(margin = margin(t = 0)),
          legend.title = element_blank(),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.895, 0.67),
          legend.box.spacing = unit(0.2, "cm"),
          panel.grid.minor = element_blank(),
          plot.title = element_text(hjust =0, size = 16),
          plot.subtitle = element_text(hjust =0, size = 12),
          plot.caption = element_text(hjust =0, size = 12),
          text = element_text(size = 16),
          axis.title = element_text(size = 16),
          axis.text = element_text(size = 16))
  
  lag_plot
  return(lag_plot)
  
}

gt_plot <- plot_delay_new(test, lag_max= 15, nuts = 1, .adjust = 0.5)
gt_plot
ggsave(plot = lag_plot, filename = paste0("figs/small_plot_lag_lvl1_gt_new.png"),
       width = 22, height = 10, units = "cm")


for (i in 1:3){
  # i = 3
  plot_lag <- plot_delay_new(all_lags_list[[i]], nuts =i, lag_max = 15,
                             .adjust = 0.75)
  # plot_lag
  ggsave(plot = plot_lag, filename = paste0("figs/small_plot_lag_lvl", i, "_op2.png"),
         width = 20, height = 10, units = "cm")
}


# simple plot to get nice legend
df_aux <- data.frame(x = c(1,2), `Empiric PDF`= c(1,2), Baseline = c(2,3), `Peak~Corr.` = c(3, 4)) |>
  pivot_longer(cols = 2:4) |>
  mutate(col = "Histogram")

df_aux$name[df_aux$name == "Empiric.PDF"] <- "Empiric PDF"
df_aux$name[df_aux$name == "Peak.Corr."] <- "Peak Correlation"

ggplot(df_aux, aes(x = x, y = value, color = name, linetype = name))+
  geom_line(linewidth = 1)+
  scale_color_manual("Legend", values=c( "blue","black", "red"))+
  scale_linetype_manual("Legend", values = c( "dotdash","solid", "dashed"))+
  geom_histogram(inherit.aes = F,
                 data = df_aux,
                 aes(x = x,
                     y = after_stat(x),
                     fill = col),
                 bins = (2+1),
                 color = "transparent",
                 alpha = 0.8)+
  scale_fill_manual("Legend", values=c("grey"))+
  theme_bw()

ggsave(filename = "figs/aux.png",
       width = 20, height = 10, units = "cm")

# 4. term comparison ------------------------------------------------------

df <- read.csv("data/multiTimeline.csv", skip=1) |>
  set_names(c("date", "Dürre", "Blitzdürre")) |>
  mutate(date = paste0(date, "-01"),
         date = as.Date(date)) |>
  pivot_longer(2:3, names_to = "term", values_to = "interest") |>
  mutate(interest = ifelse(interest == "<1", 0.5, interest),
         interest = as.numeric(interest)) 

tail(df)

ggplot(df, aes(x = date, y = interest, color = term))+
  geom_path(linewidth = 1.5)+
  scale_x_date(name = "Date", expand = c(0,0), 
               breaks = seq.Date(from = as.Date("2004-01-01"),
                                 to = as.Date("2023-12-31"),
                                 by = "4 years"),
               date_labels = "%Y")+
  scale_y_continuous(name = "Google trends index", expand = c(0,0),
                     limits = c(0,100), breaks = seq(0,100,20))+
  # scale_color_viridis_d("Term", option = "H")+
  scale_color_grey("Term", start = 0.3, end = 0.6)+
  theme_bw()+
  theme(strip.text.x = element_blank(),
        strip.background = element_rect(colour="white", fill="white"),
        legend.position=c(.1,.75),
        legend.box.background = element_rect(colour = "black"),
        plot.title = element_text(hjust =0, size = 16),
        plot.subtitle = element_text(hjust =0, size = 12),
        plot.caption = element_text(hjust =0, size = 12),
        text = element_text(size = 16))


ggsave("figs/gt_comparison.png", units = "cm", 
       width = 20, height = 10)  
 
  
     ratio_gt <- df |>
  pivot_wider(names_from = "term", values_from = "interest") |>
  mutate(ratio = Blitzdürre/Dürre) 

ratio_gt |>
  mutate(year = lubridate::year(date)) |>
  group_by(year) |>
  filter(year >= 2020) |>
  summarise_all(mean, na.trm = T) |>
  select(-date) |>
  # View() |>
  colSums()
  
  
