#### Statistics II Report ####

# This is our script for CLUSTERING
# here we apply different clustering algorithms


# QUESTION: is it possible to include the PCs in a weighted way into the clustering?
# Maybe multiply by amount of variance they account for?
# http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/
# CLUSTERING ON PCA:
# https://rpubs.com/Bury/ClusteringOnPcaResults


# packages -----------------------
install.packages("monoClust")

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
library(dendextend) # for dendrogram visualization
library(monoClust)

theme_set(theme_bw()) # set ggplot theme


# load data -----------------------

# read meteorological data and turn time into timestamp
data <- read.csv("data_clean/data.csv") |>
  mutate(time = lubridate::ym(time))
data_stats <- read.csv("data_clean/data_stats.csv")

# read metadata
station_metadata <- read.csv("data_clean/station_metadata.csv")
variables_metadata <- read.table("metadata/variables_metadata.txt", header = T, sep = ",")
station_names <- readRDS("data_clean/station_names.rds")
variables <- readRDS("data_clean/variables.rds")


# kmeans clustering -------------------

set.seed(200) # for reproducibility

# cluster based on longitude and latitude
cluster_lonlat <- kmeans(
  station_metadata |> dplyr::select(lon,lat),
  centers = 4)

# cluster based on all data_stats variables (temp, precip...)
cluster_stats <- kmeans(
  data_stats |> dplyr::select(!stn),
  centers = 4)

# cluster based on Precipitation PCA
cluster_pca <- kmeans(
  precip.pca$rotation[,1:4],
  centers = 4)

# add the cluster information to the metadata
station_metadata_cluster <- station_metadata |>
  mutate(cluster_lonlat_kmeans = factor(cluster_lonlat$cluster),
         cluster_pca_kmeans = factor(cluster_pca$cluster),
         cluster_stats_kmeans = factor(cluster_stats$cluster))



# plot kmeans clusters ---------------------

# plot map of the clusters
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

# create base plot with switzerland
plot_swiss_map <- ggplot() +
  geom_sf(data = world, colour = 'black', fill = 'white', size = 0.2) +
  coord_sf(xlim = c(5.8, 10.5), ylim = c(45.7, 47.8), expand = FALSE) +
  labs(x = "", y = "")

# show lonlat clusters
plot_swiss_map +
  geom_point(data = station_metadata, aes(x = lon, y = lat, color = cluster_lonlat_kmeans), size = 1) +
  scale_color_brewer(palette = "Set1")

# show data_stats clusters
plot_swiss_map +
  geom_point(data = station_metadata, aes(x = lon, y = lat, color = cluster_stats_kmeans), size = 1) +
  scale_color_brewer(palette = "Set1")

# show pca kmeans clusters
plot_swiss_map +
  geom_point(data = station_metadata, aes(x = lon, y = lat, color = cluster_pca_kmeans), size = 1) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Kmeans precipitation PCA clustering",
       color = "Cluster") # change legend title


# Better visualisation in multivariate predictor space - or with first two Principal components! as in Script page 11-10



# hierarchical clustering -------------------------

# correlation between original variables and PCs is given by loading matrix (if the data was scaled before)
# read more here https://stats.stackexchange.com/questions/115032/how-to-find-which-variables-are-most-correlated-with-the-first-principal-compone
# hclust in R https://www.datacamp.com/tutorial/hierarchical-clustering-R


# loadings
loadings <- precip.pca$rotation[,1:4]
loadings_df <- data.frame(loadings)
loadings_df$stn = row.names(loadings_df)

# create df with weighted PCs (multiply by amount of variance they account for) for clustering
# var_explained from precip.pca script does that
loadings_weighted <- t(t(loadings) * var_explained[1:4]) # transpose for correct multiplication
loadings_df_weighted <- data.frame(loadings_weighted)
loadings_df_weighted$stn = row.names(loadings_weighted)

# compute distance matrix
dist_mat <- dist(loadings, method = 'euclidean')
dist_mat_weighted <- dist(loadings_weighted, method = 'euclidean')

# build hierarchical clusters
station_cluster <- hclust(dist_mat, method = 'complete')
plot(station_cluster)

station_cluster_weighted <- hclust(dist_mat_weighted, method = 'complete')
plot(station_cluster_weighted) # CHD and SMA change cluster, everything else remains similar

# split into k clusters
cluster_cut <- cutree(station_cluster, k = 4)
cluster_cut_weighted <- cutree(station_cluster_weighted, k = 4)

# add cluster to station metadata
station_metadata_cluster <- station_metadata_cluster |>
  mutate(cluster_pca_h = as.factor(cluster_cut),
         cluster_pca_weighted = as.factor(cluster_cut_weighted))

# add cluster to loadings df
loadings_df$cluster = as.factor(cluster_cut)
loadings_df_weighted$cluster = as.factor(cluster_cut_weighted)

# R evaluate ideal number of clusters via inertia? no function found


# visualize the clusters ---------------------------

# plot the clusters in the dendrogram
station_dendro <- as.dendrogram(station_cluster)
station_dendro_col <- color_branches(station_dendro, k = 4) # 4 clusters
plot(station_dendro_col)


# plot clusters in 3D scatterplot
plot_ly(x = loadings_df$PC4,
        y = loadings_df$PC2,
        z = loadings_df$PC3,
        type = "scatter3d",
        mode = "markers",
        color = loadings_df$cluster)

# plot weighted clusters in 3D scatterplot
plot_ly(x = loadings_df_weighted$PC1,
        y = loadings_df_weighted$PC2,
        z = loadings_df_weighted$PC3,
        type = "scatter3d",
        mode = "markers",
        color = loadings_df_weighted$cluster)

# plot clusters in map
plot_swiss_map +
  geom_point(data = station_metadata, aes(x=lon, y=lat, color=cluster_pca_h), size=2) +
  scale_color_brewer(palette = "Set1") +
  labs(title = "Hierarchical precipitation PCA clustering",
       color = "Cluster") # change legend title

ggsave("cluster_map.png", path="figures/")
# same result as kmeans!!


# visualize loadings for the 4 PCA depending on station ------------

# create dataframe with station metadata and loadings
station_metadata_pca <- left_join(station_metadata, loadings_df)

plot_swiss_map +
  geom_point(data = station_metadata_pca, aes(x=lon, y=lat, color=PC4)) +
  scale_color_viridis()
#  labs(title = "PC3 loadings",
#       color = "loading") # change legend title

scico_palette_show()




# save data ----------

write_csv(station_metadata_cluster, "data_clean/station_metadata_cluster.csv")
write_csv(station_metadata_pca, "data_clean/station_metadata_pca.csv")

