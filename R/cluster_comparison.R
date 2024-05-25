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


# look at precip data per cluster ----------------------

# raw precip data from pca
data_precip <- data_selected |> filter(stn != "JUN")

# fill cluster column based on pca_hierarchic clusters from metadata dataframe
data_precip$cluster <- station_metadata_cluster[match(data_precip$stn, station_metadata_cluster$stn), 14]

# select all data from january
data_precip |>
  filter(month(time) == 6) |>

  # plot boxplot by cluster
  ggplot(aes(x = cluster, y = rre150m0)) +
    geom_boxplot()


# compare data between stations for all clusters
data_precip |>
  filter(month(time) == 1) |>

  # plot boxplot by cluster
  ggplot(aes(x = stn, y = rre150m0)) +
  geom_boxplot() +
  facet_wrap(~cluster, scales = "free_x")


