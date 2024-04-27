#### Statistics II Report ####

# This is our script for BASIC STATISTICS
# Here we look into basic correlations etc


# You can find ***INFO ON VARIABLE AND STATION NAMES*** in
# variables_metadata and station_metadata

scico_palette_show()

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

# read metadata
station_metadata <- read.csv("data_clean/station_metadata.csv")
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")

print(variables_metadata)

# data formating --------------------

# data in a long format (for some plots)
data_long <- data |>
  pivot_longer(cols = c(4:21),
               names_to = "variable",
               values_to = "value")

# order station column by elevation (long and short data format)
data_long$stn <- factor(data_long$stn,
                        levels = (station_metadata |> arrange(elev))$stn) # reads the station names ordered by elev

data$stn <- factor(data$stn,
                   levels = (station_metadata |> arrange(elev))$stn)


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


# first date for every station
data |>
  group_by(stn) |>
  arrange(time) |>
  slice(1L)


# easy summary plots -----------------

# filter out rows with missing values for Plot
data_stats |>
  dplyr::filter(!is.na(precip_annual)) |>

  # scatterplot temperature vs precipitation
  ggplot(aes(x=temp_mean, y=precip_annual, color=elev, label=stn)) +
    geom_point() +
    geom_text(hjust=-0.2, vjust=0) + # add text labels
    scale_color_viridis() +
    theme_classic() +
    labs(x="Mean temperature [°C]",
         y="Mean annual precipitation [mm]")


# scatterplot matrix with mean values for the stations
data_stats |>
  dplyr::select(!stn) |>
  ggpairs()


# violin plot by station (stations ordered by elevation)
data_long |>
  filter(variable == "tre200m0" | variable == "gre000m0") |>  # select here the variables you want to compare
  ggplot(aes(x=stn, y=value, fill=elev)) +
    geom_violin() +
    facet_wrap(~variable, scales = "free") + # create distinct panels for each variable
    scale_fill_viridis(direction = -1) # reversed viridis color scheme


# plot map of the stations
world <- ne_countries(scale = "medium", returnclass = "sf")

ggplot(data=world) +
  geom_sf() +
  coord_sf(xlim = c(5.5, 10.5), ylim = c(45.5, 48), expand = FALSE) +
  geom_point(data = station_metadata, aes(x=lon, y=lat, color=elev)) +
  scale_color_scico(palette = "lajolla", direction = -1, begin = 0.3, end = 0.9)

