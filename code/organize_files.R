"""
Organize files for app

Pedro Alencar
16.01.24
"""

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


# 1. import datasets ------------------------------------------------------

df1 <- readRDS("files/ufz_full_series_lvl1.RData") 
df2 <- readRDS("files/ufz_full_series_lvl2.RData")
df3 <- readRDS("files/ufz_full_series_lvl3.RData")


df1_c <-df1 |>
  filter(lubridate::year(date) >= 2004) |>
  select(nuts_id, date, fd_ratio)

df_gt <- readRDS("files/gt_full_series_lvl1.RData")

df_gt_c <- left_join(df_gt, df1_c, by = c("nuts_id", "date"), suffix = c(".gt", ".new")) |>
  mutate(fd_ratio = fd_ratio.new) |>
  select(date, nuts_id, imp_ratio, fd_ratio) |>
  saveRDS("files/gt_full_series_lvl1_2022.RData")

# check berlin ------------------------------------------------------------

df_gt_c |>
  filter(nuts_id == 'DE3') |>
  View()

df1_c |>
  filter(nuts_id == 'DE3')|>
  View()


dfa <- readRDS("app/files/gt_full_series_lvl1_2022.RData")|>
  filter(nuts_id == 'DE3')
dfb <-readRDS("app/files/ufz_full_series_lvl1_2022.RData")|>
  filter(nuts_id == 'DE3')

dfc <- readRDS("files/ufz_full_series_lvl1_2022.RData") |>
  filter(nuts_id == 'DE3')

tail(dfa, 20)
tail(dfb, 30)
tail(dfc)
