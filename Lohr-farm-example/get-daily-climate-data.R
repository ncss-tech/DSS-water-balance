library(daymetr)
library(rgdal)
library(Evapotranspiration)
library(rgeos)

source('local-functions.R')

## farm in GCS
farm <- readOGR(dsn='LohrFarm', layer='LohrFarm', stringsAsFactors = FALSE)
# center of farm for climate data queries
pt <- gCentroid(farm)

## daily climate data
dm <- getDayMet(x=coordinates(pt)[, 1], y=coordinates(pt)[, 2], start_yr = 1988, end_yr = 2018)

## estimate PET from DAYMET
ET <- estimatePET(dm, elevation = 361)

save(dm, ET, file='cached-data/daily-climate-data.rda')
