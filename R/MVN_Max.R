#### Statistics II Report ####

# This is our script for MVN tests


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
precip_data <- read.csv("data_clean/data_precip.csv")

summ_data <- summary(data)
print(summ_data)

# Specify the variables for the multivariate normality test
variables <- c("pva200m0", "gre000m0", "prestam0","tre200mx", "tre200mn", "tre200m0")

# Perform the multivariate normality test
result <- mvn(data = data[, variables], mvnTest = "hz", multivariateOutlierMethod = "quan", showOutliers = TRUE)

print(result)

# data imputation

imputed_data <- mice(data)
summary(imputed_data)

# Generate imputed datasets
imputed_datasets <- complete(imputed_data, action = "long", include = TRUE)

# The imputed datasets will be in a long format, you can convert it back to wide format if needed
# Example: convert to wide format
imputed_data_wide <- complete(imputed_data, action = "long", include = FALSE)

summary(imputed_datasets)
summary(data)

# MVN test for each individual station without imputation

mvn_results <- list()

# Loop over each station
for (station in station_names) {
  # Subset data for the current station and selected variables
  station_data <- data[data$stn == station, variables]

  # Perform MVN test
  result <- mvn(data = station_data,
                mvnTest = "hz",
                multivariateOutlierMethod = "quan",
                showOutliers = TRUE)

  # Store the result in the list
  mvn_results[[station]] <- result
}

for (i in seq(mvn_results)){
  print(mvn_results[[i]]$multivariateNormality)
}




# PRECIPITATION DATA MVN ######################################################


wide_precip <- precip_data |>
  pivot_wider(names_from = stn, values_from = rre150m0, id_cols = time) |>
  dplyr::select(-time)

mvn.precip <- mvn(data = wide_precip,
                  mvnTest = "hz",
                  multivariateOutlierMethod = "quan",
                  showOutliers = TRUE)
print(mvn.precip$multivariateNormality) # not MVN


normality_func <- function(data) {
  result_df <- data.frame(
    Variable = names(data),
    Statistic = rep(NA, ncol(data)),
    P_Value = rep(NA, ncol(data)),
    Normal = rep(NA, ncol(data))
  )

  for(i in seq_along(data)) {
    result <- shapiro.test(data[[i]])
    result_df[i, c("Statistic", "P_Value")] <- unlist(result[1:2])
    result_df[i, "Normal"] <- ifelse(result$p.value > 0.05, "Yes", "No")
  }

  return(result_df)
}

normality_precip <- normality_func(wide_precip)
