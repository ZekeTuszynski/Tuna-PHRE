
## apply phre function to sea otter data
## Max Tarjan; ltarjan@ucsc.edu
## July 29, 2014

##SD notes 2020.06.24:
# In first code block, data becomes a subset of grid, meaning zero data from 'data' remain
# Per paper: this is only viable because otters follow the coastline thus can be transformed to a 2D coastline-following reprojection
# This won't work for tuna. Why not?



library(raster)
library(rgdal)
library(ggplot2)
library(dplyr)
library(sf)

library(maptools)
library(ks)
library(spex)
library(rgeos)
library(stars)



## load data; specify file paths to supporting information


data <- read.csv("./data/F4_994_1346.csv")
# data <- cbind(data$Xcoord, data$Ycoord)
data <- cbind(data$TealeX, data$TealeY)
# grid <- read.csv("C:/Users/max/Downloads/journal.pone.0150547.s009.csv")

grid <- read.csv("./data/Tarjan&Tinker.2016.Data.csv")
#grid is background environmental data (depth, distance, study site coordinates)
#data <- subset(grid, TealeY <= 160000 & TealeY > -180000, select = c("TealeX", "TealeY"))
#data <- data[sample(x = 1:nrow(data), size = 200), ]

rasters <- list(0)
rasters[[1]] <- rasterFromXYZ(cbind(grid$TealeX, grid$TealeY, grid$ATOScal))
rasters[[2]] <- rasterFromXYZ(cbind(grid$TealeX, grid$TealeY, log(grid$Distance + 0.01)))


smoother <- cbind(c(1, 0), c(0, 0.3)) # (ATOS, 0) (0, log(dist))

source("./R/phre_function.R")
HR <- phre(locs = data, rast = rasters, smooth = smoother, percent = 90)

locations <- HR$locs

locations_df <- as.data.frame(locations)
summary(locations)

ggplot() +
  geom_point(data = locations_df , aes(x = V1, y = V2)) +
  coord_quickmap()

otterarray <- HR$array
otterarray


summary(otterarray)
summary(otterarray, maxamp = ncell(otterarray))
otterarray_df <- as.data.frame(otterarray, xy = TRUE)

# plain english notes about what every line is doing


otterpoly <- st_as_sf(HR$Polygon)


ggplot() +
  geom_raster(data = otterarray_df , aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_point(data = locations_df , aes(x = V1, y = V2, colour = "otter locations")) +
  geom_sf(data = otterpoly[[1]], colour="red", fill = NA) +
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")


locationsll <- sf::st_as_sf(locations_df, coords = c("V1", "V2"), crs = 3311) %>%
  st_transform(4326)

otterarrayll <- sf::st_as_sf(otterarray_df, coords = c("x", "y"), crs = 3311) %>%
  st_transform(4326)

sf::st_crs(otterpoly) <- 3311
otterpolyll <- sf::st_transform(otterpoly[[1]], 4326)



# from here####
hist(otterarrayll$layer)
library(tidyverse)
# Error: package or namespace load failed for ‘tidyverse’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
# there is no package called ‘reprex
# zeke to fix####
library(tidylog)
#provides feedback for dplyr functions
library(magrittr)
#introduces the %>% operator

tmp <- otterarrayll %>% # start with this object & save changes to it   otterarrayll %<>%
  drop_na(layer) %>% # remove NAs
  filter(layer > 0) # remove zeroes

tmp <- otterarrayll[which(!is.na(otterarrayll$layer) | otterarrayll$layer == 0),]
hist(expm1(tmp$layer))
which(tmp$layer == 0)

ggplot() +
  geom_sf(data = tmp , aes(colour = layer)) + # otterarrayll # background surface gradient
  scale_colour_viridis_c() +
  geom_sf(data = locationsll, colour = "black", size = 1) + # otter points
  geom_sf(data = otterpolyll, colour="red", fill = NA) + # red polygon outline
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")






# gbm.basemap####
library(gbm.auto)
library(shapefiles)
# get lat lons from otterarrayll
ottergeom <- otterarrayll %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,2],
                lon = sf::st_coordinates(.)[,1]) %>%
  dplyr::select(lat, lon) %>%
  sf::st_drop_geometry()


st_write(ottergeom,
         "./data/Crop_map.shp", driver = "ESRI Shapefile")


crop_map <- gbm.basemap(grids = ottergeom,
                        gridslat = 1,
                        gridslon = 2,
                        savedir = "C:/Users/zeket/Desktop/Coastline")

#crop_map <- read.shapefile("C:/Users/zeket/Desktop/Coastline/CroppedMap/Crop_Map")


crop_map2 <- readOGR(dsn ="C:/Users/zeket/Desktop/Coastline/CroppedMap/Crop_Map.shp")

coastdatasf <- st_as_sf(crop_map2)


#plots just the coastline data
ggplot() +
  geom_sf(data = coastdatasf, colour = "black")

#layers coastline data with PHRE map
ggplot() +
  geom_sf(data = tmp , aes(colour = layer)) + # otterarrayll # background surface gradient
  scale_colour_viridis_c() +
  geom_sf(data = coastdatasf, colour = "black", fill = "tan") +
  geom_sf(data = locationsll, colour = "black", size = 1) + # otter points
  geom_sf(data = otterpolyll, colour="red", fill = NA) + # red polygon outline
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")



# trying URL 'https://www.ngdc.noaa.gov/mgg/shorelines/data/gshhg/latest/gshhg-shp-2.3.7.zip'
# Content type 'application/zip' length 149157845 bytes (142.2 MB)
# downloaded 142.2 MB
#
# although coordinates are longitude/latitude, st_intersection assumes that they are planar
# Writing layer `Crop_Map' to data source `Crop_Map.shp' using driver `ESRI Shapefile'
# Writing 0 features with 6 fields and geometry type Unknown (any).
# Error in `[<-`(`*tmp*`, record, 1, value = readBin(infile, integer(),  :
#  subscript out of bounds
# In addition: Warning message: attribute variables are assumed to be spatially constant throughout all geometrie
