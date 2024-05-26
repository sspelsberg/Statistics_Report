#### Statistics II Report ####

# This is our script for COMPARING CLUSTER


# packages -----------------------

library(tidyverse) # dplyr etc.
library(ggplot2)
library(RColorBrewer)
library(scico)
library(plotly) # also for graphics
library(GGally) # also for graphics
library(sf) # for spatial classes
library(rnaturalearth) # for world maps etc
library(rnaturalearthdata) # for world maps etc
library(viridis) # viridis colorscales for ggplot
library(MVN) # multivariate normality checks
library(lubridate) # datetime objects

theme_set(theme_bw()) # set ggplot theme


# load data -----------------------

# read meteorological data and turn time into timestamp
data <- read.csv("data_clean/data.csv") |>
  mutate(time = lubridate::ym(time))
data_stats <- read.csv("data_clean/data_stats.csv")

# read metadata
station_metadata <- read.csv("data_clean/station_metadata.csv")
station_metadata_cluster <- read.csv("data_clean/station_metadata_cluster.csv")
station_metadata_pca <- read.csv("data_clean/station_metadata_pca.csv")
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")


# Data preparation -----------------------------

# add yearly precip to raw precip data dataframe
data_precip <- data_selected |> # raw precip data from pca
  dplyr::mutate(year = year(time), # add year and month column
                month = month(time)) |>
  dplyr::filter(year != 2024) |>
  dplyr::filter(stn != "JUN") |> # exclude Jungfraujoch (doesn't contain precip data)
  dplyr::group_by(stn, year) |>
  dplyr::mutate(precip_annual = sum(rre150m0)) |> # add column with annual precipitation per station
  dplyr::ungroup() |>
  dplyr::left_join(station_metadata_cluster |>
                     select(stn, cluster_pca_h)) |> # add cluster info
  dplyr::group_by(cluster_pca_h, year, month) |>
  dplyr::mutate(precip_cluster_mean_monthly = mean(rre150m0)) |> # compute monthly cluster mean (contains cluster timeseries)
  dplyr::ungroup()

# create new df with only the yearly values
data_precip_annual <- data_precip |>
  dplyr::select(stn, year, precip_annual, cluster_pca_h) |>
  dplyr::distinct() |> # keep only unique rows
  dplyr::group_by(cluster_pca_h, year) |>
  dplyr::mutate(precip_cluster_mean = mean(precip_annual)) |> # add column with annual cluster mean values
  dplyr::ungroup()



# Boxplots for cluster comparison ----------------------

# plot boxplot with annual data by cluster (datapoints = single stations)
data_precip_annual |>
  ggplot(aes(x = cluster_pca_h, y = precip_annual)) +
  geom_boxplot() # or geom_violin()

# plot boxplot with annual data by cluster (datapoints = annual cluster means)
data_precip_annual |>
  ggplot(aes(x = cluster_pca_h, y = precip_cluster_mean)) +
  geom_boxplot() # or geom_violin()


# boxplots for every month with individual station data
data_precip |>
  ggplot(aes(x = cluster_pca_h, y = rre150m0)) +
  geom_boxplot() +
  facet_wrap(~month(time))


# compare data between stations for all clusters
data_precip |>
  filter(month(time) == 1) |>

  # plot boxplot by cluster
  ggplot(aes(x = stn, y = rre150m0)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = "free_x")


