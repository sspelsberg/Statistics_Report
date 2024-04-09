#### Statistics II Report ####

# This is our script for DATA CLEANING
# Here we get nice data formats



# packages -----------------------

library(tidyverse) # dplyr etc.
library(ggplot2)
library(MVN) # multivariate normality checks
library(lubridate) # datetime objects

# load data ----------------------

# data problem: all stations contain different variables

# get all filenames of the single stations (all named with data)
filenames <- list.files("data_raw/", pattern = "data_*")
filepaths <- paste0("data_raw/", filenames)

# create list with all stations as single dataframes
stations <- lapply(filepaths, read.table, header = T, sep = ";")

# get metadata
station_metadata_long <- read.table("metadata/Station_metadata_long.txt", header = T)
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")


# clean climate data ---------------------

# create new list for the clean data
stations_clean <- list()

# change all integer columns to numeric, turn "-" into NA and save in new list
for (i in 1:length(stations)) {
  stations_clean[[i]] <- stations[[i]] |> mutate(across(!stn, as.numeric))
}

# combine all dataframes to one that contains all variables
data <- tibble()

for (i in 1:length(stations_clean)) {
  data <- dplyr::bind_rows(data, stations_clean[[i]])
}

# filter for a certain station and select all but one variable
data |>
  filter(stn == "ANT") |>
  select(!pva200m0)


# clean metadata -------------------------

# only keep one row per station with name, elevation and coordinates
station_metadata <- station_metadata_long |>
  dplyr::distinct(stn, name, lonlat, coord, elev) |>
  dplyr::arrange(stn)

# add elevation to climate data
data <- left_join(station_metadata |> select(stn, elev), data)

# save station names and meteorological variables
station_names <- unique(data$stn)
variables <- names(data |> select(!stn & !time & !elev)) # colnames without station name, elevation & time



# save data -------------------------------

# save data as csv
write_csv(data, "data_clean/data.csv")
write_csv(station_metadata, "data_clean/station_metadata.csv")

# save as RDS files
write_rds(station_names, "data_clean/station_names.rds")
write_rds(variables, "data_clean/variables.rds")




