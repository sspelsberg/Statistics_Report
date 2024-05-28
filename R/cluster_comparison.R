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

precip.pca$sdev^2
# load data -----------------------

# read meteorological data and turn time into timestamp
data <- read.csv("data_clean/data.csv") |>
  mutate(time = lubridate::ym(time))
data_stats <- read.csv("data_clean/data_stats.csv")
data_precip <- read.csv("data_clean/data_precip.csv") |>
  mutate(cluster_pca_h = as.factor(cluster_pca_h),
         time = lubridate::ymd(time))
data_precip_annual <- read.csv("data_clean/data_precip_annual.csv") |>
  mutate(cluster_pca_h = as.factor(cluster_pca_h))
data_clim_diagram <- read.csv("data_clean/data_clim_diagram.csv")

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

# create df for climate diagram per cluster (mean monthly precip and temp)
data_clim_diagram <- data_precip |>
  dplyr::left_join(data |> select(stn, time, tre200m0)) |>
  dplyr::group_by(cluster_pca_h, month) |>
  dplyr::summarise(mean_precip = mean(rre150m0),
                   mean_temp = mean(tre200m0))

# add yearly cluster mean to data_precip
data_precip <- data_precip |>
  dplyr::left_join(data_precip_annual) |>
  dplyr::rename(precip_cluster_mean_annual = precip_cluster_mean)


# climate diagrams for the 4 clusters -----------------------

data_clim_diagram |>
  ggplot(aes(x = month, y = mean_temp)) +
  geom_hline(yintercept = 0, linewidth = 0.3) + # black line at 0
  geom_col(aes(y = mean_precip * 0.5), fill = "#2b94c4", alpha = 0.5) + # add precip bars and set transparency to 0.5
  geom_line(color = "#d91f00", linewidth = 0.8) + # add temperature data
  facet_wrap(~cluster_pca_h, nrow = 1) + # split by cluster
  scale_y_continuous("Mean Temperature [°C]",
                     sec.axis = sec_axis(~ . * 2, name = "Mean Precipitation [mm]")) + # add the second y axis
  scale_x_continuous(breaks = c(1:12), expand = c(0.01,0.01)) + # modify x ticks and remove space left and right
  theme(panel.grid.major = element_blank()) # remove parts of the grid

ggsave("cluster_climate_diagram.svg", path="figures/", width = 7, height = 2)

# compute annual mean temp and precip sum
data_clim_diagram |>
  dplyr::group_by(cluster_pca_h) |>
  dplyr::summarise(annual_temp = mean(mean_temp),
            annual_precip = sum(mean_precip))


# Boxplots for cluster comparison ----------------------

# plot boxplot with annual data by cluster (datapoints = single stations)
data_precip_annual |>
  ggplot(aes(x = cluster_pca_h, y = precip_annual)) +
  geom_boxplot() # or geom_violin()

# plot boxplot with annual data by cluster (datapoints = annual cluster means)
data_precip_annual |>
  ggplot(aes(x = cluster_pca_h, y = precip_cluster_mean)) +
  geom_boxplot() # or geom_violin()


# boxplots for every month with individual station data (e.g. 4 station january measurements per year from 20 years)
data_precip |>
  ggplot(aes(x = cluster_pca_h, y = rre150m0)) +
  geom_boxplot() +
  facet_wrap(~month(time))

# boxplots for every month with cluster mean data (e.g. 20 january cluster means from different years)
data_precip |>
  ggplot(aes(x = cluster_pca_h, y = precip_cluster_mean_monthly)) +
  geom_boxplot() +
  facet_wrap(~month)


# boxplot for individual annual station measurements within the clusters
data_precip_annual |>
  ggplot(aes(x = stn, y = precip_annual)) +
  geom_boxplot() +
  facet_wrap(~cluster_pca_h, scales = "free_x")


# save data ----------

write_csv(data_precip, "data_clean/data_precip.csv")
write_csv(data_precip_annual, "data_clean/data_precip_annual.csv")
write_csv(data_clim_diagram, "data_clean/data_clim_diagram.csv")



