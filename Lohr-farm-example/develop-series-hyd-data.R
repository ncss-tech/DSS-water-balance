library(aqp)
library(soilDB)
library(reshape2)
library(rgdal)
library(raster)
library(rasterVis)
library(viridis)

source('local-functions.R')

# c/o Suzann
# buffered and UTM 
ssurgo <- readOGR('.', layer='ssurgo_map', stringsAsFactors = FALSE)

## TODO:
# 1. consider using RSS map

## cached soil data
load('cached-data/ssurgo-component-data.rda')


## simplified SSURGO
r <- raster('results/simplified_ssurgo_single_series.grd')
rat <- levels(r)[[1]]

# add bulk hyd data to RAT
rat <- merge(rat, s$agg, by.x='series', by.y='compname', all.x=TRUE, sort=FALSE)

# ack, re-arrange column names
nm <- names(rat)
rat <- rat[, c(grep('ID', nm), grep('ID', nm, invert = TRUE))]

# pack RAT
levels(r)[[1]] <- rat

# check: OK
levelplot(r, att='series', main='Simplified SSURGO (~30m)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=brewer.pal(5, 'Set1'),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)

# save intermediate results
writeRaster(r, file='results/simplified_ssurgo_single_series_hyd_data.grd', overwrite=TRUE)

## TODO: add sat, fc, pwp for soil moisture classification
## develop grids
# terms for leaky bucket model
soil_depth_mm <- deratify(r, att='corrected_depth') * 10
Sb <- deratify(r, att='Sb')
FC <- deratify(r, att='FC')
# used for classification of VWC
vwc.sat <- deratify(r, att='sat')
vwc.fc <- deratify(r, att='fc')
vwc.pwp <- deratify(r, att='pwp')

writeRaster(soil_depth_mm, file='results/soil_depth_mm.grd', overwrite=TRUE)
writeRaster(Sb, file='results/Sb.grd', overwrite=TRUE)
writeRaster(FC, file='results/FC.grd', overwrite=TRUE)

writeRaster(vwc.sat, file='results/vwc_sat.grd', overwrite=TRUE)
writeRaster(vwc.fc, file='results/vwc_fc.grd', overwrite=TRUE)
writeRaster(vwc.pwp, file='results/vwc_pwp.grd', overwrite=TRUE)


## Figures for presentation

levelplot(soil_depth_mm, main='Simplified SSURGO (~30m)\nWater Balance Domain (mm)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=rev(viridis(20)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)

levelplot(vwc.fc, main='Simplified SSURGO (~30m)\nField Capacity',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=rev(viridis(20)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)
