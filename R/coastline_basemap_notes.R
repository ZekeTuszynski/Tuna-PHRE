# Basemap notes, Simon Dedman, simondedman@gmail.com 2021-09-16
install.packages("gbm.auto")
library(gbm.auto)
gbm.basemap # read up on this.
# returns shapefile via shapefiles::read.shapefile
# but also saves shapefile which can be read into R other ways e.g. sf::st_read
# e.g. from that helpfile:
nc <- st_read(system.file("shape/nc.shp", package = "sf")) # modify this obviously


# Other option via maptools, probably easier:

# wrld_simpl {maptools}	R Documentation
# Simplified world country polygons
# Description
# The object loaded is a SpatialPolygonsDataFrame object containing a slightly modified version of Bjoern Sandvik's improved version of world\_borders.zip - TM\_WORLD\_BORDERS\_SIMPL-0.2.zip dataset from the Mapping Hacks geodata site. The country Polygons objects and the data slot data frame row numbers have been set to the ISO 3166 three letter codes.
#
# Usage
# data(wrld_simpl)
# Format
# The format is: Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots; the data clot contains a data.frame with 246 obs. of 11 variables
#
# The object is in geographical coordinates using the WGS84 datum.
#
# Source
# Originally “http://mappinghacks.com/data/TM_WORLD_BORDERS_SIMPL-0.2.zip”, now available from https://github.com/nasa/World-Wind-Java/tree/master/WorldWind/testData/shapefiles

data(wrld_simpl)
plot(wrld_simpl)

# https://stackoverflow.com/a/35564737/3975144
# Create a raster template for rasterizing the polys. Set grid resolution with res (degrees)
r <- raster(xmn = -180, xmx = 180, ymn = -90, ymx = 90, res = 0.5) # whole globe. 1deg: 512kb. 0.1deg = 49.5mb
r <- raster(xmn = -100, xmx = 40, ymn = 0, ymx = 65, res = 0.5) # NAtlantic subset 1deg: 512kb. 0.1deg = 49.5mb but distance() hangs
r2 <- rasterize(wrld_simpl, r, 1) # values 1 for land, NA for ocean
d <- distance(r2) # Calculate distance to nearest non-NA pixel i.e. land. 0.1 & 0.25 res hangs.
writeRaster(d, filename = paste0(machine, "Blocklab/MapData/DST_NAtl_05Deg"), format = "GTiff") # save object
d <- raster(paste0(machine, "Blocklab/MapData/DST_NAtl_05Deg.tif")) # read it back in later.
levelplot(d / 1000, # Plot. d/1000 = results units are km
  margin = FALSE,
  at = seq(0, maxValue(d) / 1000, length = 100),
  colorkey = list(height = 0.6),
  main = "Distance to coast"
)
