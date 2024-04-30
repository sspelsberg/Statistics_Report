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


# Select only the necessary columns
data_selected <- data %>% select(time, stn, rre150m0)

# Pivot the dataframe
pivot_data <- data_selected %>%
  pivot_wider(names_from = stn, values_from = rre150m0)

# Display the transformed dataframe
print(pivot_data)
