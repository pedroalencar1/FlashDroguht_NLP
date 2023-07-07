"
Study the delay between increase of FD affected area and media report

Using data from UFZ and nuts 1, 2 and 3

Pedro Alencar
07.07.2023
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

source("../../@R scripts/Utilities.R")
shape_nuts_lvl <- sf::read_sf("data/GIS/NUTS/NUTS_RG_20M_2021_4326.shp") |> 
  filter(CNTR_CODE == "DE")


# 1. get data -------------------------------------------------------------

df_lvl1 <- readRDS(file = "files/ufz_full_series_lvl1.RData")
df_lvl2 <- readRDS(file = "files/ufz_full_series_lvl2.RData")
df_lvl3 <- readRDS(file = "files/ufz_full_series_lvl3.RData")

units_l1 <- unique(df_lvl1$nuts_id)
units_l2 <- unique(df_lvl2$nuts_id)
units_l3 <- unique(df_lvl3$nuts_id)


#' function to get percentile series
#' 
#' Requires functions `get_percentiles` and `perc_from_series` available in 
#' the `delay_analysis.R` file
get_perc_lvl <- function(df_lvl, units_lvl){
  
  df_perc <- data.frame()
  pb <- easy_progress_bar(length(units_lvl))
  for (unit_id in units_lvl){
    pb$tick()
    df_unit <- get_percentiles(df = df_lvl, 
                               unit = unit_id)
    
    df_perc <- rbind(df_perc, df_unit)
  }
  
  return(df_perc)
  
}

df_perc_lvl1 <- get_perc_lvl(df_lvl1, units_l1)
df_perc_lvl2 <- get_perc_lvl(df_lvl2, units_l2)
df_perc_lvl3 <- get_perc_lvl(df_lvl3, units_l3)



# 2. run lags -------------------------------------------------------------

#' function to obtain lag between event and impact
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
    
    all_lags$lag[i] <- (val1)
    all_lags$ccf[i] <- (val2)
  
  }
  
  return(all_lags)
}

all_lags_lvl1 <- get_all_lags(df_perc_lvl1, units_l1, lag_max = 10)
all_lags_lvl2 <- get_all_lags(df_perc_lvl2, units_l2)
all_lags_lvl3 <- get_all_lags(df_perc_lvl3, units_l3)

all_lags_list <- list(all_lags_lvl1,
                      all_lags_lvl2,
                      all_lags_lvl3)

saveRDS(all_lags_list, file = "files/all_lags_15.RData")


# 3. plot delays ----------------------------------------------------------

#' function to plot kernel distribution and histogram of delays
plot_delay <- function(all_lags, lag_max = 15, nuts = 1){
  
  colors <- c("Histogram" = "#999999", 
              "Empiric PDF" = "#000000", 
              "Baseline" = "blue")
  
  
  all_lags_aux <- all_lags |>
    filter(lag > 0) |>
    mutate(lag = lag - lag_max) 
  
  # get intercept of baseline
  stat <- density(all_lags_aux$lag)
  df_stat <- data.frame(x = stat$x,
                        y = stat$y) |>
    filter(x < 0) |>
    filter(x > -1*lag_max)
  
  base_line_y <- mean(df_stat$y)
  
  # get plot
  lag_plot <- all_lags_aux |>
    ggplot(aes(x = lag-1))+
    geom_histogram(aes(y = after_stat(density),
                       fill = "Histogram"),
                   bins = (2*lag_max+1), 
                   color = "#999999") +
    geom_density(aes(color = "Empiric PDF"),
                 adjust = 1/2.5)+
    geom_hline(aes(color = "Baseline",
                   yintercept = base_line_y), 
               linetype="dotdash",
               linewidth = 1)+
    scale_y_continuous(expand = c(0,0), 
                       limits = c(0, 0.1),
                       breaks = seq(0,0.2, 0.02))+
    scale_x_continuous(expand = c(0,0),
                       breaks = seq(-20,20,5))+
    scale_color_manual("Legend",values = colors)+
    scale_fill_manual("", values=colors)+
    labs(x = "Delay (weeks)",
         y = "Density",
         title = "Distribution of delay between FD onset and impact perception",
         subtitle = paste0("Data aggregated into NUTS-", nuts),
         caption = "Delay measured in weeks.
                  Negative delays indicate baseline concern on droughts (blue line).",
         tag = "Legend")+
    theme_bw()+
    theme(legend.position = 'right', 
          legend.spacing.x = unit(0.2, 'cm'),
          legend.spacing.y = unit(-0.2, 'cm'),
          legend.text = element_text(margin = margin(t = 0)),
          legend.title = element_blank(),
          plot.tag = element_text(size = 11),
          plot.tag.position = c(0.895, 0.67),
          legend.box.spacing = unit(0.2, "cm"),
          panel.grid.minor = element_blank())
  
  return(lag_plot)
  
}

for (i in 1:3){
  plot_lag <- plot_delay(all_lags_list[[i]], nuts =i, lag_max = 15)
  ggsave(plot = plot_lag, filename = paste0("figs/plot_lag_lvl", i, ".png"),
         width = 20, height = 12, units = "cm")
}


# 4. check each nuts1 -----------------------------------------------------
all_lags_lvl3 <- all_lags_list[[3]]

list_lags_nuts1 <- list()
list_plots_nuts1 <- list()

for (nuts in units_l1){
  
  nuts_name <- shape_nuts_lvl$NUTS_NAME[which(shape_nuts_lvl$NUTS_ID == nuts)]
  
  list_lags_nuts1[[nuts]] <- all_lags_lvl3 |>
    filter(grepl(nuts, unit))
  
  list_plots_nuts1[[nuts]] <- plot_delay(list_lags_nuts1[[nuts]],
                                         nuts = paste0("1 (", nuts_name, ")"))
  
  ggsave(plot = list_plots_nuts1[[nuts]],
         filename = paste0("figs/plot_lab_lvl1_",nuts,".png"),
         width = 20, height = 12, units = "cm")
  
}


# 4.1 Join small states ---------------------------------------------------
shape_nuts_lvl |> filter(LEVL_CODE == 1)

#berlin brandenburg
lags_bbr <- rbind(list_lags_nuts1[["DE3"]],
                  list_lags_nuts1[["DE4"]])
plot_delay(lags_bbr,
           nuts = paste0("1 (Berlin-Brandenburg)"))

ggsave(plot = last_plot(),
       filename = paste0("figs/plot_lab_lvl1_BerlinBrandenburg.png"),
       width = 20, height = 12, units = "cm")

#bremen, hamburg, niedersachsen, schleswig
lags_NE <- rbind(list_lags_nuts1[["DE5"]],
                 list_lags_nuts1[["DE6"]],
                 list_lags_nuts1[["DEF"]],
                 list_lags_nuts1[["DE9"]])

plot_delay(lags_NE,
           nuts = paste0("1 (NE Germany)"))

ggsave(plot = last_plot(),
       filename = paste0("figs/plot_lab_lvl1_NE_Germany.png"),
       width = 20, height = 12, units = "cm")

#Saarland Rheinland
lags_bbr <- rbind(list_lags_nuts1[["DEC"]],
                  list_lags_nuts1[["DEB"]])
plot_delay(lags_bbr,
           nuts = paste0("1 (Saarland-Rheinland)"))

ggsave(plot = last_plot(),
       filename = paste0("figs/plot_lab_lvl1_SaarlandRheinland.png"),
       width = 20, height = 12, units = "cm")

