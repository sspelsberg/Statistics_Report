#### Statistics II Report ####

# This is Sophies script for playing around with the PCA results




# visualize loadings for the 4 PCA depending on station ------------


# create dataframes with station metadata and loadings
station_metadata_pca <- left_join(station_metadata, loadings_df)
station_metadata_pca_long <- station_metadata_pca |>
  select(stn,lon,lat,elev,PC1,PC2,PC3,PC4) |>
  pivot_longer(cols = c(PC1, PC2, PC3, PC4),
               names_to = "PC",
               values_to = "loading")

# create base plot with switzerland
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")

plot_swiss_map <- ggplot() +
  geom_sf(data = world, colour = 'black', fill = 'white', size = 0.2) +
  coord_sf(xlim = c(5.8, 10.5), ylim = c(45.7, 47.8), expand = FALSE) +
  labs(x = "", y = "")


# Plot PCA loadings
plot_swiss_map +
  geom_point(data = station_metadata_pca, aes(x=lon, y=lat, color=PC3)) +
  scale_color_viridis(limits=c(-0.425, 0.465))
#  labs(title = "PC3 loadings",
#       color = "loading") # change legend title


# plot all 4 PCs
plot_swiss_map +
  geom_point(data = station_metadata_pca_long, aes(x=lon, y=lat, color=loading)) +
  scale_color_viridis() +
  facet_wrap(~PC)

ggsave("loading_map.png", path="figures/")

scico_palette_show()


# plot the PCA timeseries -----------------------------

# extract the principal components and create df with them
test.princomps = data.frame(precip.pca$x)
test.princomps <- test.princomps |>
  mutate(time = seq(ymd("2000-01-01"),ymd("2024-01-01"),by='months'),
         month = month(time),
         year = as.factor(year(time)))


# Plot Principal Component timeseries
ggplot(data = test.princomps, aes(x=time)) +
  geom_line(aes(y=PC1), color = "lightblue")+
  geom_line(aes(y=PC2), color = "lightgreen")+
  geom_line(aes(y=PC3), color = "yellow")+
  geom_line(aes(y=PC4), color = "orange")+
  labs(y="scaled precipitation")


# Plot PC timeseries but look for annual pattern
ggplot(data = test.princomps, aes(x=month, y=PC1, color=year))+
  geom_line()+
  labs(y="scaled precipitation")



# shrinked scree plot -----------------------

# scree plot
qplot(c(1:p), var_explained) +
  geom_col() +

  geom_text(aes(label = round(var_explained, 2)), vjust = -0.5) +
  xlab("Principal Component") +
  ylab("Variance Explained") +
  ylim(0, 0.65)

ggsave("screeplot_small.svg", path = "figures/", width = 7, height = 2.5)




