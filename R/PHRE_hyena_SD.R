## PHRE in hyenas
## Prepared by M Tarjan
## June 11, 2019

## INPUTS
## locs: dataframe with colnames x & y
## rast: raster with 1 or more layers
## smoother: bandwidth value (matric or scalar) or 'default'
## percent: percent kernel to calculate (e.g., 90)

# Edits by Simon Dedman simondedman@gmail.com 2020-06-30 & later

# load & prep input data ####

library(maptools) ## required for spatialpoints
library(raster)

## prepare inputs
# data <- read.csv("C:/Users/max/Desktop/Tarjan/hyena_data/OHB08.csv")
library(sf)
library(tidyverse)
library(magrittr)
library(rgdal)
# machine <- "/home/simon/Documents/Si Work/" #Aquarius

# AllDailies <- readRDS(paste0(machine, "Blocklab/abft_diving/All_Daily/AllDailies_HIFSDA_Stocknames.Rds"))
# maploadloc = "/home/simon/Dropbox/Blocklab Monterey/Data/" #ensure trailing /slash
# GSL <- st_read(paste0(maploadloc, "Maps/GulfOfStLawrence/GulfOfStLawrence.gpkg"))
# AllDailies %<>%
#   drop_na(Date, lat, lon) %>%
#   filter(Stock %in% c("GOM", "Med")) %>%
#   select(lat, lon, Stock)
# sfAllDailies <- sf::st_as_sf(AllDailies, coords = c("lon", "lat")) %>% sf::st_set_crs(4326) #points by day
# sfAllDailiesInGSL <- sfAllDailies[GSL,] #sf objects subset points occurring in poly
# plot(data$LON, data$LAT)

# locs.ll <- subset(data, select = c("LON", "LAT"))
# locs.ll <- subset(AllDailies, select = c("lon","lat"))
# locs.ll <- SpatialPoints(locs.ll)
# locs.ll <- as_Spatial(sfAllDailiesInGSL)
# crs(locs.ll) <- CRS("+init=epsg:4326") ## assign a coordinate system
# # locs.utm <- spTransform(locs.ll, CRSobj = CRS("+init=epsg:32733")) ## reproject
# locs.utm <- spTransform(locs.ll, CRSobj = CRS("+init=epsg:6931")) ## reproject
# writeOGR(obj = locs.utm,
#          dsn = "/home/simon/Dropbox/Blocklab Monterey/Data/Maps/GulfOfStLawrence/tempdir.gpkg",
#          layer = "locs.utm",
#          driver = "GPKG")
locs.utm <- readOGR("./data/tempdir.gpkg")

# bbox(locs.utm)
# min      max
# coords.x1 -4271641 -3730767
# coords.x2 -2576010 -1632010

# fence <- rgdal::readOGR(dsn = "C:/Users/max/Desktop/Tarjan/hyena_data/Fences/Fences", layer = "Fences")
# out.fence.utm <- rgdal::readOGR(dsn = "C:/Users/max/Desktop/Tarjan/hyena_data/Fences/Fences", layer = "outside_fences_utm2")

# get high quality coastline map shape
natlantic <- read_sf("./data/Crop_Map.shp") # HQ enough??
# natlantic <- st_crop(natlantic,
#                      xmin = -98,
#                      ymin = 8,
#                      xmax = 36,
#                      ymax = 65)
natlantic <- st_crop(natlantic, # GSL only
                     xmin = -71,
                     ymin = 45,
                     xmax = -55,
                     ymax = 52)
# Error in s2_geography_from_wkb(x, oriented = oriented, check = check) :
#   Evaluation error: Found 1 feature with invalid spherical geometry.
# [441] Loop 0 is not valid: Edge 9754 has duplicate vertex with edge 9759.

# convert coast to raster ####
library(stars)
# coast.rast <- st_rasterize(natlantic, st_as_stars(st_bbox(natlantic), nx = 20000, ny = 20000, values = NA_real_)) # resolution maximised to 32gb RAM constraints
# coast.rast <- st_rasterize(natlantic, st_as_stars(st_bbox(natlantic), nx = 1000, ny = 1000, values = NA_real_)) # resolution maximised to 32gb RAM constraints
coast.rast <- raster(as(natlantic, "Spatial"), ncols = 100, nrows = 100)
coast.rast <- rasterize(as(natlantic, "Spatial"), coast.rast, getCover = TRUE, progress = "text")
# plot(coast.rast, axes = TRUE)
# write_stars(obj = coast.rast,
# dsn = paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map_Raster.tif"))
# coast.rast <- read_stars(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map_Raster.tif"))


## reproject to projected coordinate system
## original - EPSG:4326 WGS 84.  convert- EPSG:32733 UTM 33S
# raster::crs(fence)
# fence.utm <- spTransform(fence, CRSobj = CRS("+init=epsg:32733"))
# out.fence.utm <- spTransform(out.fence.utm, CRSobj = CRS("+init=epsg:32733"))
library(rgdal)
raster::crs(coast.rast)
crs(coast.rast) <- CRS("+init=epsg:4326")
# coast.rast <- spTransform(coast.rast, CRSobj = CRS("+init=epsg:6931"))
coast.rast <- projectRaster(coast.rast,
                            crs = CRS("+init=epsg:6931"))
# coast.rast <- st_transform(coast.rast, crs = st_crs(6931))
# write_stars(obj = coast.rast,
#             dsn = paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map_Raster_crs6931.tif"))
# coast.rast <- read_stars(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map_Raster_crs6931.tif"))

# plot(out.fence.utm)
# plot(locs.utm, add = T)


# plot(locs.ll)
plot(coast.rast)
plot(locs.utm, add = T)


# create habitat rasters ####
# bbox <- summary(out.fence.utm)$bbox
bbox <- bbox(extent(coast.rast))
# habitat <- raster(xmn = bbox[1, 1] + 500,
#                   xmx = bbox[1, 2] - 500,
#                   ymn = bbox[2, 1] + 500,
#                   ymx = bbox[2, 2] - 500,
#                   crs = crs(out.fence.utm),
#                   resolution = 50)
habitat <- raster(xmn = bbox[1, 1] + 500,
                  xmx = bbox[1, 2] - 500,
                  ymn = bbox[2, 1] + 500,
                  ymx = bbox[2, 2] - 500,
                  crs = crs(coast.rast),
                  resolution = 50)
# dd <- rgeos::gDistance(spgeom1 = out.fence.utm,
#                        spgeom2 = as(habitat, "SpatialPoints"),
#                        byid = T) ## calc distance from fence for every point in raster
dd <- rgeos::gDistance(spgeom1 = coast.rast,
                       spgeom2 = as(habitat, "SpatialPoints"),
                       byid = T) ## calc distance from fence for every point in raster
# Error in (function (classes, fdef, mtable):unable to find an inherited method for function ‘is.projected’ for signature ‘"RasterLayer"’
dd <- distanceFromPoints(object = coast.rast,
                         xy = as(habitat, "SpatialPoints"))
# Never completed, ran for hours
# need to extract a df of values from dd?
ddvec <- extract(dd)

habitat[] <- log(apply(dd, 1, min) + 0.01)
coast.rast[] <- log(apply(ddvec, 1, min) + 0.01) # Error in dimnames(x) <- list(n) : 'dimnames' applied to non-array
plot(habitat)
plot(fence.utm, add = T)
plot(locs.utm, add = T)

plot(dd)
plot(coast.rast, add = T)
plot(locs.utm, add = T)

# plot(HR$Poly); plot(habitat, add=T); plot(fence.utm, add=T)

rasters <- list(x = habitat, y = habitat, z = habitat)
rasters[[1]][] <- rasterToPoints(habitat)[, 1]
rasters[[2]][] <- rasterToPoints(habitat)[, 2]
# rasters[[3]]<-habitat

rm(habitat, dd) ## remove habitat from workspace to save space

# run PHRE ####
source("./R/phre_function.R")
## apply phre function
HR <- phre(locs = locs.utm@coords,
           rast = rasters,
           smooth = "default",
           percent = 90,
           resolution = 1500)
# HR<-phre(locs=locs.utm@coords, rast=rasters)##resolution here is equal to the number of cells across the input raster, so a higher number leads to better resolution; default is 500

# gc() ##garbage collection to free up memory

## for trouble-shooting in phre function
# locs<-locs.utm@coords; rast<-rasters; smooth<-'default'; percent=90; resolution=1500

# plot phre list objects ####
## zoomed in on home range
plot(HR$Poly)
plot(HR$array, add = T)
plot(HR$Polygon, col = "transparent", border = "red", add = T)
points(HR$locs, pch = 20, cex = .1, col = "black")
plot(fence.utm, add = T)

## zoom on park
plot(fence.utm)
plot(HR$array, add = T)
plot(HR$Polygon, col = "transparent", border = "red", add = T)
points(HR$locs, pch = 20, cex = .1, col = "black")
plot(fence.utm, add = T)
