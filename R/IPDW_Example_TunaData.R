# IPDW 2020.06.24 ####
# Inverse Path Distance Weighting spatial interpolation
# Interpolates data from points, e.g. salinity, DST, and respects coastlines
# NOT useful for home range estimation, even though it's similar to Kernel Interpolation With Barriers, in ArcGIS,
# available from the Geostatistical analyst toolbox in ArcGIS 10.1
# (https://onlinelibrary-wiley-com.stanford.idm.oclc.org/doi/10.1111/mms.12260)
# IPDW lacks the kernel bit hence it's a flat interpolation not a density estimator.
# install.packages("ipdw")
library(ipdw)
pols <- # gpkg coastline, SpatialPolygonsDataFrame object
  library(sf)
machine <- "/home/simon/Documents/Si Work/" #Aquarius
natlantic <- read_sf(paste0(machine, "Blocklab/iccat_SSM_data/outputs/CroppedMap/Crop_Map.shp"))
class(natlantic) # "sf"         "tbl_df"     "tbl"        "data.frame"
pols <- as(natlantic, 'Spatial')

# pnts <- # matrix of coordinates or a SpatialPointsDataFrame object
loadloc = paste0(machine, "Blocklab/abft_diving/All_Daily/") #per saveloc in MolaFoldersExtractLoop.R
AllDailies <- readRDS(paste0(loadloc, "AllDailies_HIFSDA_Stocknames.Rds"))
library(magrittr)
library(dplyr)
library(tidyverse)
AllDailies %<>% drop_na(Date, lat, lon) # omit rows with NA values
sfAllDailies <- sf::st_as_sf(AllDailies, coords = c("lon","lat")) %>%
  sf::st_set_crs(4326) #points by day
class(sfAllDailies) # "sf"         "data.frame"
pnts <- as(sfAllDailies, 'Spatial')

costras <- costrasterGen(xymat = pnts,
                         pols = pols,
                         extent = "pnts",
                         projstr = projection(pols),
                         resolution = 0.1)


# find average nearest neighbor
# install.packages("spatstat")
library(spatstat)

W              <- owin(range(coordinates(pnts)[,1]), range(coordinates(pnts)[,2]))
kat.pp         <- ppp(coordinates(pnts)[,1], coordinates(pnts)[,2], window = W)
mean.neighdist <- mean(nndist(kat.pp))

# grid building
gridsize       <- mean.neighdist * 2 # 0.1210296
grainscale.fac <- gridsize / res(costras)[1] # 0.1213485
gridras        <- raster::aggregate(costras, fact = grainscale.fac) # fact = grainscale.fac but needs to be positive integer
gridpol        <- rasterToPolygons(gridras)
gridpol$value  <- row.names(gridpol)

# spatial join
fulldataset.over    <- over(pnts, gridpol)
# fulldataset.over    <- cbind(data.frame(fulldataset.over),
#                              setNames(data.frame(pnts),
#                                       c("id", "salinity", "x.utm", "y.utm", "optional")))
fulldataset.over    <- cbind(data.frame(fulldataset.over),
                             setNames(data.frame(pnts),
                                      c(colnames(AllDailies)[-c(2, 3)], "lon", "lat", "optional"))) # remove lat lon, add at end

# grid selection
set.seed(2)
# install.packages("gdata")
library(gdata)
gridlev <- unique(fulldataset.over$value)
for (i in seq_along(gridlev)) {
  activesub <- subset(fulldataset.over, fulldataset.over$value == gridlev[i])
  selectnum <- gdata::resample(seq_len(nrow(activesub)), 1)
  if (i == 1) {
    training <- activesub[selectnum,]
  }
  else{
    training <- rbind(training, activesub[selectnum,])
  }
}

validate             <- fulldataset.over[!(row.names(fulldataset.over) %in%
                                             row.names(training)),]
# xy                   <- cbind(training$x.utm, training$y.utm)
xy                   <- cbind(training$lon, training$lat)
training             <- SpatialPointsDataFrame(xy, training)
# xy                   <- cbind(validate$x.utm, validate$y.utm)
xy                   <- cbind(validate$lon, validate$lat)
validate             <- SpatialPointsDataFrame(xy, validate)
projection(training) <- projection(pnts)
projection(validate) <- projection(pnts)
plot(costras)
points(training)
points(validate, col = "red")


# paramlist <- c("salinity")
paramlist <- c("DistanceToShoreKm")
final.ipdw <- ipdw(training,
                   costras,
                   range = mean.neighdist * 10,
                   paramlist,
                   overlapped = TRUE)
# plot(final.ipdw, main = "Kattegat salinity (ppt)")
plot(final.ipdw, main = "North Atlantic Distance To Shore (Km)")
# class(costras)
# class(validate)
# don't know how to turn final.ipdw into a df of points or anything else.
