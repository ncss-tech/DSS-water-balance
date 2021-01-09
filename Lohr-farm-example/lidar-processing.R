
# https://github.com/Jean-Romain/lidR/wiki/Rasterizing-perfect-canopy-height-models

## do we need all of these tiles?


library(rgdal)
library(lidR)

## CRS of lidar
# https://www.sciencebase.gov/catalog/file/get/58279062e4b01fad87046320?f=__disk__4d%2F9a%2Fa5%2F4d9aa516a6a86e96e5cf1ee39171ac437d8fe63d&transform=1&allowOpen=true
# 
# Standard Parallel: 38.03333333333333
# Standard Parallel: 39.2
# Longitude of Central Meridian: -78.5
# Latitude of Projection Origin: 37.66666666666666
# False Easting: 11482916.66666666
# False Northing: 6561666.666666666

lcc <- '+proj=lcc +lat_1=38.03333333333333 +lat_2=39.2 +lat_0=37.66666666666666 +lon_0=-78.5 +x_0=11482916.66666666 +y_0=6561666.666666666 +datum=NAD83 +units=m +no_defs'

# ?`LAScatalog-class`
# Build a catalog from downladed .laz files
ctg <- readLAScatalog("E:/gis_data/VA/")

# set CRS
projection(ctg) <- CRS(lcc)

# farm outline
farm <- readOGR(dsn='LohrFarm', layer='LohrFarm', stringsAsFactors = FALSE)
farm_lcc <- spTransform(farm, CRS(lcc))

# CRS transform seems to work
mapview::mapview(farm_lcc)

# CRS of LIDAR clearly incorrect, or tiles are named incorrectly
mapview::mapview(ctg)





# check: coordinates aren't right: are these the wrong tiles?
plot(ctg)
plot(farm_lcc, add=TRUE, border='red', lwd=2)

# 
# likely much faster in GRASS
z <- grid_terrain(ctg, res=1, tin())
plot(z, col = height.colors(50))

## WTF?

## looks right, but the coordinates are totally wrong
writeRaster(z, filename = 'LiDAR-wrong-CRS.tif', options=c("COMPRESS=LZW"))



