
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
library(magrittr)
library(tidyverse)
# Error: package or namespace load failed for ‘tidyverse’ in loadNamespace(j <- i[[1L]], c(lib.loc, .libPaths()), versionCheck = vI[[j]]):
# there is no package called ‘reprex
# zeke to fix####
library(tidylog)


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

# in projected CRS
ggplot() +
  geom_raster(data = otterarray_df , aes(x = x, y = y, fill = layer)) +
  scale_fill_viridis_c() +
  geom_point(data = locations_df , aes(x = V1, y = V2, colour = "otter locations")) +
  geom_sf(data = otterpoly[[1]], colour = "red", fill = NA) +
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")


# Convert to lat lon
locationsll <- sf::st_as_sf(locations_df, coords = c("V1", "V2"), crs = 3311) %>%
  st_transform(4326)

otterarrayll <- sf::st_as_sf(otterarray_df, coords = c("x", "y"), crs = 3311) %>%
  st_transform(4326)

sf::st_crs(otterpoly) <- 3311
otterpolyll <- sf::st_transform(otterpoly[[1]], 4326)

hist(otterarrayll$layer) # loads of zeroes & NAs

otterarrayll %<>% # start with this object & save changes to it   otterarrayll %<>%
  drop_na(layer) %>% # remove NAs
  filter(layer > 0) # remove zeroes

# in Lat Lon
ggplot() +
  geom_sf(data = otterarrayll , aes(colour = layer)) + # background surface gradient
  scale_colour_viridis_c() +
  geom_sf(data = locationsll, colour = "black", size = 1) + # otter points
  geom_sf(data = otterpolyll, colour = "red", fill = NA) + # red polygon outline
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")

# get lat lons from otterarrayll
ottergeom <- otterarrayll %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,2], # 2021-10-21 lat lon were the wrong way around
                lon = sf::st_coordinates(.)[,1]) %>%
  dplyr::select(lat, lon) %>%
  sf::st_drop_geometry()

st_write(ottergeom,
         "./data/otterpoints.shp",
         driver = "ESRI Shapefile")


# Add basemap
# gbm.basemap####
library(gbm.auto)
# from here####
dir.create("../basemap")
# library(rgdal) # if running gbm.basemap locally while fixing it
# library(maptools)
# library(raster)
# library(graphics)
# library(sf)
# library(shapefiles)
# library(rgeos)
crop_map <- gbm.basemap(grids = ottergeom,
                        gridslat = 1,
                        gridslon = 2,
                        savedir = "../basemap") # "/home/simon/Dropbox/Blocklab Monterey/Internships_Teaching_Recruitment/Zeke Tuszynski/basemap"
class(crop_map) # list, per gbm.basemap call:
# cropshp <- read.shapefile(savename) # read it back in with read.shapefile which results in the expected format for draw.shape in mapplots, used in gbm.map # shapefiles::
# We want it as an sf object for ggplot, so:
crop_map2 <- st_read(dsn = paste0("../basemap/CroppedMap/Crop_Map.shp"), layer = paste0("Crop_Map"), quiet = TRUE) # read in worldmap
class(crop_map2) # "sf"         "data.frame"
# redo map, add basemap

options(scipen = 5) # avoids exponentiated numbers in legend

# in Lat Lon
ggplot() +
  geom_sf(data = otterarrayll , aes(colour = layer)) + # background surface gradient
  scale_colour_viridis_c() +
  geom_sf(data = locationsll, colour = "yellow", size = 1) + # otter points
  geom_sf(data = otterpolyll, colour = "red", fill = NA) + # red polygon outline
  geom_sf(data = crop_map2, colour = "grey", fill = "grey") + # coastline basemap
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")

#To do####
# Crop basemap extents to otterarrayll extents
# change legend name from "layer"
# white background with grey lines (i.e. reverse of current)
# subtitle explaining red polygon outline, yellow dots, layer gradient
