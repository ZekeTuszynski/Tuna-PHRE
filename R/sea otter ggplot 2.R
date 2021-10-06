
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


#defines sftc_as_cols function and uses it to split geometry column in otterarrayll
#into x and y columns


sfc_as_cols <- function(x, geometry, names = c("x","y")) {
  if (missing(geometry)) {
    geometry <- sf::st_geometry(x)
  } else {
    geometry <- rlang::eval_tidy(enquo(geometry), x)
  }
  stopifnot(inherits(x,"sf") && inherits(geometry,"sfc_POINT"))
  ret <- sf::st_coordinates(geometry)
  ret <- tibble::as_tibble(ret)
  stopifnot(length(names) == ncol(ret))
  x <- x[ , !names(x) %in% names]
  ret <- setNames(ret,names)
  dplyr::bind_cols(x,ret)
}


otterarrayll2 <- sfc_as_cols(otterarrayll, geometry, names = c("x","y"))




library(stars)
ggplot() +
  geom_raster(data = otterarrayll2, aes(x=x, y=y, fill = layer)) + #x = x, y = y,
  #  geom_raster requires the following missing aesthetics: x and y
  scale_fill_viridis_c() +
  geom_point(data = locationsll, aes(colour = "otter locations")) + #x = V1, y = V2,
  geom_sf(data = otterpolyll, colour="red", fill = NA) +
  ggtitle("Big Sur Sea Otter Home Range") + xlab("Longitude") + ylab("Latitude")

# > otterarrayll
# Simple feature collection with 343176 features and 1 field
# Geometry type: POINT
# Dimension:     XY
# Bounding box:  xmin: -121.546 ymin: 35.92165 xmax: -121.4674 ymax: 36.01771
# Geodetic CRS:  WGS 84
# First 10 features:
#   layer                   geometry
# 1  8.391191e-14  POINT (-121.546 36.01672)
# 2  1.559140e-13 POINT (-121.5458 36.01672)
# 3  1.320022e-13 POINT (-121.5456 36.01672)
# 4  1.102612e-13 POINT (-121.5455 36.01673)
# 5  9.234535e-14 POINT (-121.5453 36.01673)
# 6  7.789295e-14 POINT (-121.5451 36.01673)
# 7  6.644651e-14  POINT (-121.545 36.01673)
# 8  5.750272e-14 POINT (-121.5448 36.01674)
# 9  4.322516e-14 POINT (-121.5447 36.01674)
# 10 1.420815e-14 POINT (-121.5445 36.01674)


# gbm.basemap####
library(gbm.auto)
# get lat lons from otterarrayll
ottergeom <- otterarrayll %>%
  dplyr::mutate(lat = sf::st_coordinates(.)[,1],
                lon = sf::st_coordinates(.)[,2]) %>%
  dplyr::select(lat, lon) %>%
  sf::st_drop_geometry()


crop_map <- gbm.basemap(grids = ottergeom,
                        gridslat = 1,
                        gridslon = 2)

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
