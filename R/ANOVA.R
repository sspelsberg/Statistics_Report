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
library(multcomp)
library(MASS)
library(car)
library(rstatix)

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


# Reduce the data to keep unique annual cluster means
reduced_data <- data_precip_annual |>
  dplyr::distinct(precip_cluster_mean, .keep_all = TRUE)


# Check assumptions for ANOVA ##############################################

reduced_data |>
  ggplot(aes(x=precip_cluster_mean, color=cluster_pca_h))+
  geom_density()

# Visualize the data
reduced_data |>
  ggplot(aes(x=factor(cluster_pca_h), y=precip_cluster_mean)) +
  geom_boxplot() +
  labs(x="Cluster", y="Precipitation") +
  theme_minimal()

reduced_data |>
  ggplot(aes(sample = precip_cluster_mean)) +
  geom_qq() +
  geom_qq_line() +
  facet_wrap(~ cluster_pca_h, scales = "free")


# Shapiro test for normality
shapiro_test_results <- reduced_data %>%
  group_by(cluster_pca_h) %>%
  summarise(p_value = shapiro.test(unique(precip_cluster_mean))$p.value)
print(shapiro_test_results)

# Levene test for homogeneity of variances across clusters
leveneTest(precip_annual ~ factor(cluster_pca_h), data = reduced_data)

# Pairwise testing of homogeneous variance
clusters <- unique(reduced_data$cluster_pca_h)

# Generate all unique pairs of clusters
cluster_pairs <- combn(clusters, 2, simplify = FALSE)

# Function to perform Levene's test on a pair of clusters
perform_levene_test <- function(pair, data) {
  cluster1 <- pair[1]
  cluster2 <- pair[2]

  # Filter data for the two clusters
  data_pair <- data %>% filter(cluster_pca_h %in% c(cluster1, cluster2))

  # Perform Levene's test
  test_result <- leveneTest(precip_annual ~ factor(cluster_pca_h), data = data_pair)

  # Extract p-value and test statistic
  p_value <- test_result$`Pr(>F)`[1]
  f_value <- test_result$`F value`[1]

  # Return a data frame with the results
  return(data.frame(cluster1 = cluster1, cluster2 = cluster2, F_value = f_value, p_value = p_value))
}

# Apply the function to each pair and combine results into a single data frame
levene_results <- do.call(rbind, lapply(cluster_pairs, perform_levene_test, data = reduced_data))

# Print the results
print(levene_results)

# ASSUMPTION OF HOMOGENEOUS VARIANCE NOT FULFILLED !!! #########################

# ANOVA performed anyway ######################################################
anova_result <- aov(precip_cluster_mean ~ factor(cluster_pca_h), data = reduced_data)
summary(anova_result)

clusters <- unique(reduced_data$cluster_pca_h)

# Function to perform ANOVA on every pair
perform_anova <- function(pair, data) {
  cluster1 <- pair[1]
  cluster2 <- pair[2]

  # Filter data for the two clusters
  data_pair <- data %>% filter(cluster_pca_h %in% c(cluster1, cluster2))

  # Perform ANOVA
  test_result <- aov(precip_annual ~ factor(cluster_pca_h), data = data_pair)
  summary_result <- summary(test_result)

  # Extract p-value and test statistic
  p_value <- summary_result[[1]][["Pr(>F)"]][1]
  f_value <- summary_result[[1]][["F value"]][1]

  # Return a data frame with the results
  return(data.frame(cluster1 = cluster1, cluster2 = cluster2, F_value = f_value, p_value = p_value))
}

# Apply the function to each pair and combine results into a single data frame
anova_results <- do.call(rbind, lapply(cluster_pairs, perform_anova, data = reduced_data))

# Print the results
print(anova_results)


# Welche's ANOVA ######################################################

# Perform Welch's ANOVA
welch_anova <- oneway.test(precip_annual ~ cluster_pca_h, data = reduced_data, var.equal = FALSE)

# Print the results
print(welch_anova)

# Function to perform Welch's t-test on every pair
perform_welch_ttest <- function(pair, data) {
  cluster1 <- pair[1]
  cluster2 <- pair[2]

  # Filter data for the two clusters
  data_pair <- data %>% filter(cluster_pca_h %in% c(cluster1, cluster2))

  # Perform Welch's t-test
  test_result <- t.test(precip_annual ~ factor(cluster_pca_h), data = data_pair, var.equal = FALSE)

  # Extract p-value and test statistic
  p_value <- test_result$p.value
  t_value <- test_result$statistic

  # Return a data frame with the results
  return(data.frame(cluster1 = cluster1, cluster2 = cluster2, t_value = t_value, p_value = p_value))
}

# Apply the function to each pair and combine results into a single data frame
welch_ttest_results <- do.call(rbind, lapply(cluster_pairs, perform_welch_ttest, data = reduced_data))

# Print the results
print(welch_ttest_results)

# Games Howell test
GH_test <- games_howell_test(reduced_data, formula = precip_annual~cluster_pca_h)
print(GH_test)

# Define sample sizes and variances for each group (Example variances)
n <- 24
k <- 4
variances <- c(1.5, 2.0, 2.5, 3.0)  # Replace with actual variances

# Calculate the weights
weights <- variances / n

# Calculate the numerator for the denominator degrees of freedom
numerator <- sum(weights)^2
denominator <- sum((weights^2) / (n - 1))

# Calculate Welch's denominator degrees of freedom
df2 <- round(numerator / denominator)

# Numerator degrees of freedom for ANOVA
df1 <- k - 1
# plot an F distribution


# Generate a sequence of x values (F-statistic values)
x <- seq(0, 5, length.out = 100)

# Compute the corresponding y values (density of the F distribution)
y <- df(x, df1, df2)

# Create a data frame for plotting
data <- data.frame(x = x, y = y)

# Calculate the critical value for the 5% rejection region
alpha <- 0.05
f_critical <- qf(1 - alpha, df1, df2)


ggplot(data, aes(x = x, y = y)) +
  geom_line(color = "black") +
  geom_area(data = subset(data, x >= f_critical), aes(x = x, y = y), fill = "red", alpha = 0.5) +
  labs(x = "F value",
       y = "Density") +
  geom_segment(aes(x = f_critical, y = 0, xend = f_critical, yend = max(y) / 3.3), linetype = "dashed", color = "red") +
  annotate("text", x = f_critical + 0.2, y = max(y) / 3, label = paste("Critical value =", round(f_critical, 2)), color = "red") +
  annotate("text", x = f_critical + 1, y = max(y) / 7, label = paste("alpha =", alpha), color = "red")+
  theme_bw()

