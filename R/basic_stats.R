#### Statistics II Report ####

# This is our script for BASIC STATISTICS
# Here we look into basic correlations etc


# You can find ***INFO ON VARIABLE AND STATION NAMES*** in
# variables_metadata and station_metadata



# packages -----------------------

library(tidyverse) # dplyr etc.
library(ggplot2)
library(viridis) # viridis colorscales for ggplot
library(MVN) # multivariate normality checks
library(lubridate) # datetime objects


# load data -----------------------

# read meteorological data and turn time into timestamp
data <- read.csv("data_clean/data.csv") |>
  mutate(time = lubridate::ym(time))

# read metadata
station_metadata <- read.csv("data_clean/station_metadata.csv")
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")


# stats summary -------------------------

summary(data)

# build summary with mean (or total) values
data_stats <- data |>
  group_by(stn) |>
  dplyr::summarise(elev = mean(elev),
                   temp_mean = mean(tre200m0, na.rm = T),
                   precip_annual = mean(rre150m0, na.rm = T)*12,
                   press_mean = mean(pva200m0, na.rm = T),
                   rad_mean = mean(gre000m0, na.rm = T),
                   snow_annual = mean(hns000m0, na.rm = T)*12)


# filter out rows with missing values for Plot
data_stats |>
  filter(!is.na(precip_annual)) |>

  # scatterplot temperature vs precipitation
  ggplot(aes(x=temp_mean, y=precip_annual, color=elev, label=stn)) +
    geom_point() +
    geom_text(hjust=-0.2, vjust=0) + # add text labels
    scale_color_viridis() +
    theme_classic() +
    labs(x="Mean temperature [°C]",
         y="Mean annual precipitation [mm]")


