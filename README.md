# Statistics_Report
Our repository for the Statistics Report


---------------------------------

Git commands: 

step 1: go into the git folder Statistics_Report

download: 
1) git pull

upload: 
1) git add . 
2) git commit -m "here goes my message"
3) git push


---------------------------------
### Analysis To Dos ###

Dataset: 
precipitation data from all stations except Jungfraujoch
stations = 19 variables
precip measurements = monthly precipitation totals for ca. 20 yrs --> 240 observations

Basics:
- MVN --> check (only for this precipitation dataset)
- maybe check basic correlations with geographic variables (elevation, lon, lat) and/or temperature, pressure etc.

Analysis:
1) PCA: reduce number of STATIONS --> identify main patterns in the precipitation
plot loadings in a biplot, plot maps as in paper?
2) CLUSTERING: build clusters based on the correlation with the PCAs (see paper)
3) TIMESERIES: identify patterns in the clustered data: trends, seasonality... and compare among clusters
- also possible: check for stationarity
- also possible: no timeseries analysis but t-test (or however you call that stuff) to see whether means, variance etc. among clusters are significantly different

Goal: 
Identify spatiotemporal patterns in swiss hydroclimate and define regions of equal precipitation regimes!
Possible discussion: compare our regions with the regions that resulted from the 1960 data (paper).


### additional stuff #####

Additional meteoswiss data at
data - https://www.meteoschweiz.admin.ch/service-und-publikationen/applikationen/ext/climate-normtables.html 

station and climate zone metadata - https://www.meteoschweiz.admin.ch/klima/klima-der-schweiz/rekorde-und-extreme/extremwertanalysen/hintergrundinformationen/stationsuebersicht.html 
