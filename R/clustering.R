#### Statistics II Report ####

# This is our script for CLUSTERING
# here we apply different clustering algorithms


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
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")


# kmeans clustering -------------------

set.seed(200) # for reproducibility

# cluster the data based on longitude and latitude
cluster_lonlat <- kmeans(
  station_metadata |> dplyr::select(lon,lat),
  centers = 5)

# cluster the data based on all data_stats variables
cluster_stats <- kmeans(
  data_stats |> dplyr::select(!stn),
  centers = 5)

# add the cluster information to the original dataframes
station_metadata$cluster_lonlat <- factor(cluster_lonlat$cluster)
data_stats$cluster_stats <- factor(cluster_stats$cluster)


# plot kmeans clusters ---------------------

# plot map of the clusters
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# create base plot with switzerland
plot_swiss_map <- ggplot() +
  geom_sf(data = world, colour = 'black', fill = 'white', size = 0.2) +
  coord_sf(xlim = c(5.8, 10.5), ylim = c(45.7, 47.8), expand = FALSE) +
  labs(x = "", y = "")

# show lonlat clusters
plot_swiss_map +
  geom_point(data = station_metadata, aes(x = lon, y = lat, color = cluster_lonlat), size = 1) +
  scale_color_brewer(palette = "Set1")

# show data_stats clusters
plot_swiss_map +
  geom_point(data = data_stats, aes(x = lon, y = lat, color = cluster_stats), size = 1) +
  scale_color_brewer(palette = "Set1")


# Better visualisation in multivariate predictor space - or with first two Principal components! as in Script page 11-10



# clustering from exercise -------------------------


