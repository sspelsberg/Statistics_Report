#### Statistics II Report ####

# This is our script for NORM DATA
# Here we look at additional meteoswiss data 1961-1990


# packages -----------------------

library(tidyverse) # dplyr etc.
library(ggplot2)
library(MVN) # multivariate normality checks
library(lubridate) # datetime objects

# load data ----------------------

read.table("data_raw/climate-reports-normtables_prestam0_1961-1990_de.txt", header = T, sep = ",")

# get all filenames of the meteoswiss files that contain normtables 1961 - 1990
filenames <- list.files("data_raw/", pattern = "climate-reports-normtables*")
filepaths <- paste0("data_raw/", filenames)

filepaths[1]

# create list with all climate normtables
# normtables <- lapply(filepaths, read.table, header = T, sep = ";")
