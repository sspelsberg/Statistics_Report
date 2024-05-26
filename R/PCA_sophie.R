#### Statistics II Report ####

# This is Sophies script for playing around with the PCA results


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




