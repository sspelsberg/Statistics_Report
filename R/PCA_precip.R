#### Statistics II Report ####

# This is our script for precipitation PCA


# packages -----------------------

library(tidyverse) # dplyr etc.
library(ggplot2)
library(plotly) # also for graphics
library(GGally) # also for graphics
library(sf) # for spatial classes
library(rnaturalearth) # for world maps etc
library(rnaturalearthdata) # for world maps etc
library(viridis) # viridis colorscales for ggplot
library(MVN) # multivariate normality checks
library(lubridate) # datetime objects
library(mice) # data imputation


theme_set(theme_bw()) # set ggplot theme

# load data -----------------------

# read meteorological data and turn time into timestamp
data <- read.csv("data_clean/data.csv") |>
  mutate(time = lubridate::ym(time))

# read metadata
station_metadata <- read.csv("data_clean/station_metadata.csv")
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")

summ_data <- summary(data)
print(summ_data)


# Select only the necessary columns for precip PCA
data_selected <- data %>% select(time, stn, rre150m0)

# Pivot the dataframe
pivot_data <- data_selected %>%
  pivot_wider(names_from = stn, values_from = rre150m0)

# remove Jungfrau data

pivot_data <- pivot_data |> select(c(-JUN, -time))


# Display the transformed dataframe
print(pivot_data)

p <- ncol(pivot_data) # number of stations

# standardize data (not sure it's really necessary, but prob yes because variance should not be equal btw stations)
scaled_data <- scale(pivot_data)

precip.pca <- prcomp(scaled_data)
summary(precip.pca)
var_explained <- precip.pca$sdev^2 / sum(precip.pca$sdev^2)

qplot(c(1:p), var_explained) +
  geom_col() +
  geom_text(aes(label = round(var_explained, 2)), vjust = -0.5) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ggtitle("Scree Plot") +
  ylim(0, 1)


precip.weights <- data.frame(precip.pca$rotation)
pc1SortedWeights <- precip.weights[order(precip.weights[, 1], decreasing = TRUE),][, 1, drop = F]
pc2SortedWeights <- precip.weights[order(precip.weights[, 2], decreasing = TRUE),][, 2, drop = F]
