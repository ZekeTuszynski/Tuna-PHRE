## apply phre function to sea otter data
## Max Tarjan; ltarjan@ucsc.edu
## July 29, 2014

##SD notes 2020.06.24:
# In first code block, data becomes a subset of grid, meaning zero data from 'data' remain
# Per paper: this is only viable because otters follow the coastline thus can be transformed to a 2D coastline-following reprojection
# This won't work for tuna. Why not?

## load data; specify file paths to supporting information
data <- read.csv("./data/F4_994_1346.csv") # actual otter data
data <- cbind(data$TealeX, data$TealeY) # cbind(data$Xcoord, data$Ycoord)
# or use random points instead of actual data
# data <- subset(grid, TealeY <= 160000 & TealeY > -180000, select = c("TealeX", "TealeY"))
# data <- data[sample(x = 1:nrow(data), size = 200), ] ## create random points in range of grid for locations
grid <- read.csv("./data/Tarjan&Tinker.2016.Data.csv")



## create list of rasters from the grid array
library(raster)
library(maptools)
library(ks)
library(spex)
library(rgeos)
rasters <- list(0)
rasters[[1]] <- rasterFromXYZ(cbind(grid$TealeX, grid$TealeY, grid$ATOScal))
rasters[[2]] <- rasterFromXYZ(cbind(grid$TealeX, grid$TealeY, log(grid$Distance + 0.01)))

## specify the baseline smoothing parameters
smoother <- cbind(c(1, 0), c(0, 0.3)) # (ATOS, 0) (0, log(dist))

## adapt the ATOS smoothing parameter based on the nearest neighbor distances between re-sight locations
# library(amap)
# dist<-as.matrix(Dist(data))
# nn<-dim(0)
# for (c in 1:dim(dist)[2]) {
#  min.temp<-min(dist[which(dist[,c]!=0),c])
#  nn<-c(nn,min.temp)
# }
# mean.nn<-mean(log(nn))
# smoother[1,1]<-smoother[1,1]*2*(mean.nn/4)^2.5

## load the phre function
source("./R/phre_function.R")
## apply phre function
HR <- phre(locs = data, rast = rasters, smooth = smoother, percent = 90)

## plot of phre list objects
## zoomed in on polygons
plot(HR$Poly)
plot(HR$array, add = T) #predictive surface: predicted otter abundance
plot(HR$Polygon, col = "transparent", border = "red", add = T) #same as line 50 Polygon == poly
points(HR$locs, pch = 20, cex = .1, col = "black") #actual otter sightings




# Generate coastline from data extents ####
dir.create("./data/coastline")
# install.packages("gbm.auto")
# grids example until latlon grids file exists
gridsexample <- data.frame(lat = c(36, 37), lon = c(-122.27, -121.74))

coast <- gbm.auto::gbm.basemap( # IDK if you need to assign this to coast, think you might be able to just call it
  bounds = c(),
  grids = gridsexample, # name your grids database here: lat-lon transformed data powering 'array' above
  gridslat = 1, # lat col number in grids
  gridslon = 2, # lon col number in grids
  getzip = TRUE,
  zipvers = "2.3.7",
  savedir = tempdir(),
  # savename = "./data/coastline/Crop_Map",
  savename = "/home/simon/Desktop/Crop_Map",
  res = "CALC",
  extrabounds = TRUE
)

# read in crop_map with:
coast <- sf::st_read(dsn = "./data/coastline/Crop_Map.shp", layer = "Crop_Map", quiet = TRUE) # read in worldmap
coast <- sf::st_read(dsn = "/home/simon/Desktop/Crop_Map.shp", layer = "Crop_Map", quiet = TRUE) # read in worldmap
library(ggplot2)
library(sf)
library(ggspatial)
ggplot2::ggplot() +
  layer_spatial(coast, fill = "grey", lwd = 0)
  # or add to existing plot with annotation_spatial() instead

# IDK why crop_map isn't updating, is still north canada. maybe have to delete it each time? overwreite required?

#add coastline next
#x and y axes
# bathymetry possibly
# legend title
# move everything into ggplot
# SD to get hammerhead data & coast data
