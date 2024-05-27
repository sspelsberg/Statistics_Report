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


data_precip_annual |>
  ggplot(aes(x=precip_annual, color=cluster_pca_h))+
  geom_density()

# Visualize the data
data_precip_annual |>
  ggplot(aes(x=factor(cluster_pca_h), y=precip_annual)) +
  geom_boxplot() +
  labs(x="Cluster", y="Precipitation") +
  theme_minimal()


data_precip_annual |>
  ggplot(aes(sample = precip_annual)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ cluster_pca_h, scales = "free")

shapiro_test_results <- data_precip_annual %>%
  group_by(cluster_pca_h) %>%
  summarise(p_value = shapiro.test(precip_annual)$p.value)
print(shapiro_test_results)

