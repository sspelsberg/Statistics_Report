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
library(gridExtra) # grid plots


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
# SOPHIE: scaling is necessary for PCA (at least if the ranges of the variables are very different, but this is probably not given here)
scaled_data <- scale(pivot_data)

precip.pca <- prcomp(scaled_data)
summary(precip.pca)
var_explained <- precip.pca$sdev^2 / sum(precip.pca$sdev^2)

# scree plot
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

# plot the PCs

biplot(precip.pca)

# biplot without sample points
biplot(precip.pca, cex = 0, xlabs = rep("", nrow(scaled_data)),
       xlim = c(0,0.25))


precip.weights


# select PCAs ----------

# mean amount of variance explained by one variable
100 / 19 # 5.26

# would result in first 4 PCAs

# Kaiser's rule (variance explained >1 would result in first 8 PCAs)


# plotting the loadings of each station for the first 4 PCs

rotation_matrix <- precip.pca$rotation[, 1:4]

rotation_df <- as.data.frame(rotation_matrix) %>%
  rownames_to_column(var = "Sample")

# Reshape the data to long format using gather (tidyr)
rotation_df_long <- rotation_df %>%
  gather(key = "variable", value = "value", -Sample)


# Plot each principal component separately by sample
plots <- list()
#color palette setting
viridis_palette <- viridis_pal(begin=0.1,end=0.4)(4)

for (i in 1:4) {
  pc <- paste0("PC", i)
  pc_data <- rotation_df_long %>%
    filter(variable == pc)

  p <- ggplot(pc_data, aes(x = Sample, y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = pc, x = "", y = "") +
    scale_fill_manual(values = viridis_palette[i]) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
          axis.ticks.x = element_blank())+
    guides(fill=F)

  if (i %in% c(3, 4)) {
    p <- p + labs(x = "Samples")  # Add x-axis label "Samples" to the bottom plots
  }

  if (i %in% c(1, 3)) {
    p <- p + labs(y = "PCA weights")  # Add y-axis label "PCA weights" to the left plots
  }


  plots[[i]] <- p
}

grid.arrange(grobs=plots, ncol=2)

# Plot each principal component separately by value
plots <- list()
for (i in 1:4) {
  pc <- paste0("PC", i)
  pc_data <- rotation_df_long %>%
    filter(variable == pc) %>%
    arrange(abs(value))  # Arrange by the absolute values of 'value'

  p <- ggplot(pc_data, aes(x = reorder(Sample, value), y = value, fill = variable)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = pc, x = "", y = "") +
    scale_fill_manual(values = rainbow(1)) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))

  if (i %in% c(3, 4)) {
    p <- p + labs(x = "Samples")  # Add x-axis label "Samples" to the bottom plots
  }

  if (i %in% c(1, 3)) {
    p <- p + labs(y = "PCA weights")  # Add y-axis label "PCA weights" to the left plots
  }

  plots[[i]] <- p
}

grid.arrange(grobs=plots, ncol=2)
