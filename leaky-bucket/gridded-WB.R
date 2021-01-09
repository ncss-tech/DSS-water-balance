library(raster)
library(hydromad)
library(aqp)
library(soilDB)
library(rgdal)
library(progress)
library(rasterVis)
library(viridis)
# library(scico)

source('local-functions.R')

## local SSURGO map
# c/o Suzann
# buffered and UTM 
ssurgo <- readOGR('.', layer='ssurgo_map', stringsAsFactors = FALSE)

# ## daily climate data
# load('cached-data/daily-climate-data.rda')
# 
# ## subset to 2 years of climate data

## TODO: load from cached data instead ##############
##
## use wb.series

library(daymetr)
library(Evapotranspiration)
## farm in GCS
farm <- readOGR(dsn='LohrFarm', layer='LohrFarm', stringsAsFactors = FALSE)
# center of farm for climate data queries
pt <- rgeos::gCentroid(farm)

## daily climate data
dm <- getDayMet(x=coordinates(pt)[, 1], y=coordinates(pt)[, 2], start_yr = 2015, end_yr = 2015)

## estimate PET from DAYMET
ET <- estimatePET(dm, elevation = 361)

## TODO: load from cached data instead ##############



## load rasters
twi.norm <- raster('DEM-and-derived/normalized_saga_twi.tif')
abr.norm <- raster('DEM-and-derived/normalized_abr.tif')
soil_depth_mm <- raster('results/soil_depth_mm.grd')
Sb <- raster('results/Sb.grd')
FC <- raster('results/FC.grd')
vwc.sat <- raster('results/vwc_sat.grd')
vwc.fc <- raster('results/vwc_fc.grd')
vwc.pwp <- raster('results/vwc_pwp.grd')

## distribute PPT
# replicate TWI for all days
ppt.stack <- calc(twi.norm, fun = function(i) {rep(i, times=nrow(dm))})
# multiply by PPT vector, neat 
ppt.stack <- ppt.stack * dm$prcp..mm.day.

## distribute PET via normalized annual beam radiance
# expand ET into a stack
et.stack <- calc(abr.norm, fun = function(i) {rep(i, times=length(ET$ET.Daily))})
et.stack <- et.stack * as.vector(ET$ET.Daily)


## figures for demonstration
idx <- 4
value.idx <- dm$prcp..mm.day.[idx]
levelplot(ppt.stack[[idx]], main='Distributed PPT (mm)\nvia SAGA TWI',
          sub=sprintf('DAYMET PPT: %s (mm)', value.idx),
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)

idx <- 120
value.idx <- round(ET$ET.Daily[idx])
levelplot(et.stack[[idx]], main='Distributed PET (mm)\nvia Ann. Beam Radiance',
          sub=sprintf('Est. PET: %s (mm)', value.idx),
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)


## compute all pieces of WB
## translate into dry|moist|saturated|free water

## compute annual water balance at each grid cell:
## distributed PPT via normalized TWI
## daily ET
## series-specific hydraulic parameters

# ## TODO: figure out how to return individual components of the WB
# 
# pixel.fun <- function(ppt, et) {
#   
#   if(all(is.na(ppt))) {
#     wb <- rep(NA, times=length(ppt))
#   } else {
#     wb <- WB.simple(PPT = as.vector(ppt), PET = as.vector(et), D = 1:length(ppt), 
#                     Sb = 250, fc=0.73, thick_mm = 300,
#                     S_0 = 1, a.ss = 0.03, M = 0, etmult = 1, a.ei = 0
#     )
#   }
#   
#   ## how to safely return a matrix
#   return(wb$S)
# }
# 
## TODO:  this isn't working, bug?
#
# multivariate stack algebra with overlay
# https://stackoverflow.com/questions/38404112/stackapply-with-multivariate-function-in-r
# overlay(PPT.dist, ET, function(...) {})
# wb.stack <- overlay(ppt.stack, et.stack, fun=Vectorize(pixel.fun))

## work-around?
# https://stackoverflow.com/questions/50452135/thornthwaite-evapotranspiration-on-a-raster-dataset-error-formula-not-vectoris/51643106#51643106

## init empty stacks
S.stack <- brick(ppt.stack, values=FALSE)
VWC.stack <- brick(ppt.stack, values=FALSE)
U.stack <- brick(ppt.stack, values=FALSE)
state.stack <- brick(ppt.stack, values=FALSE)

# copy dates into names
names(S.stack) <- format(dm$date, "%b %d %Y")
names(VWC.stack) <- format(dm$date, "%b %d %Y")
names(U.stack) <- format(dm$date, "%b %d %Y")
names(state.stack) <- format(dm$date, "%b %d %Y")


## ~ 10 minutes
## can it run in parallel?

# pre-compute number of iterations and make a fancy progress bar
pb <- progress_bar$new(total = ncell(ppt.stack))
n <- nlayers(ppt.stack)

# for (i in sample(1:ncell(ppt.stack), size = 100)) {
for(i in 1:ncell(ppt.stack)) {
  
  # write NA to those cells where we have no soil data
  if(is.na(Sb[i])) {
    na.vect <- rep(NA, times=n)
    S.stack[i] <- na.vect
    VWC.stack[i] <- na.vect
    U.stack[i] <- na.vect
    state.stack[i] <- na.vect
    
  } else {
    
    # run water balance using distributed PPT and PET, and soil-specific hydraulic data
    wb <- WB.simple(PPT = as.vector(ppt.stack[i]), PET = as.vector(et.stack[i]), D = 1:n, 
                    Sb = Sb[i], fc=FC[i], thick_mm = soil_depth_mm[i],
                    S_0 = 1, a.ss = 0.03, M = 0, etmult = 1, a.ei = 0
    )
    
    # soil moisture state classification
    # integer codes for factor levels
    state.stack[i] <- moistureState(VWC = wb$VWC, U = wb$U, fc = vwc.fc[i], sat = vwc.sat[i], pwp = vwc.pwp[i])
    
    # save results to each stack
    S.stack[i] <- wb$S
    VWC.stack[i] <- wb$VWC
    U.stack[i] <- wb$U
  }
  
  # progress bar
  pb$tick()
}
rm(pb)


# ## assign factor levels to stack?
# ss <- state.stack
# for(i in nlayers(ss)) {
#   ss[[i]] <- ratify(ss[[i]])
#   rat <- levels(ss[i])[[1]]
# }
# 

## TODO: save as grids so that layer names are preserved
# save stacks
writeRaster(VWC.stack, filename = 'results/VWC_time-series.grd', overwrite=TRUE)
writeRaster(S.stack, filename = 'results/S_time-series.grd', overwrite=TRUE)
writeRaster(U.stack, filename = 'results/U_time-series.grd', overwrite=TRUE)

# when using INT1U, must specify NODATA
NAvalue(state.stack) <- 0
writeRaster(state.stack, filename = 'results/moisture_state_time-series.grd', datatype='INT1U', overwrite=TRUE, NAflag=0)

# save distributed  PPT / PET stacks
writeRaster(ppt.stack, filename = 'results/distributed-PPT_time-series.grd', overwrite=TRUE)
writeRaster(et.stack, filename = 'results/distributed-PET_time-series.grd', overwrite=TRUE)

## check: OK

#... I can't believe that worked, how can we do this in parallel
#... it would solve the 'return multiple values per pass' problem by initing several stacks

## cached copies
# VWC.stack <- stack('VWC_time-series.tif')
# state.stack <- stack('moisture_state_time-series.tif')

# ms.colors <- rev(scico(6, palette='vik'))
ms.colors <- brewer.pal(6, 'Spectral')

idx <- 499:522
idx <- 230:245

levelplot(state.stack[[idx]], 
          margin=FALSE, scales=list(draw=FALSE),
          at=0:6, col.regions=ms.colors,
          # panel=function(...) {
          #   panel.levelplot(...)
          #   sp.polygons(ssurgo, col='white', lwd=1)
          # }
)


levelplot(VWC.stack[[idx]], 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis
)

levelplot(VWC.stack[[idx]], 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)


levelplot(VWC.stack[[1]], main='VWC',
          sub=gsub('X', '', names(VWC.stack[[1]])),
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='white', lwd=1)
          }
)


## demos

## temporary hack!!
elev <- raster('DEM-and-derived/elev_3m_utm.tif')

# mask elev for contours
ssurgo$mask <- 1
ssurgo.rast <- rasterize(ssurgo, elev, field='mask')
elev <- mask(elev, ssurgo.rast)
contours <- rasterToContour(elev, nlevels=10)



idx <- c(1, 64, 128, 192, 256, 320)

png(file='figures/VWC-ts-demo-1.png', width = 1200, height=800, res=96, antialias = 'cleartype')
levelplot(VWC.stack[[idx]], main='Volumetric Water Content',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=rev(viridis(20)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(contours, col='white', lwd=1)
          }
)
dev.off()

png(file='figures/U-ts-demo-1.png', width = 1200, height=800, res=96, antialias = 'cleartype')
levelplot(U.stack[[idx]], main='Estimated Ponding / Runoff (mm)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=rev(viridis(20)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(contours, col='black', lwd=1)
          }
)
dev.off()



png(file='figures/moisture-state-ts-demo-1.png', width = 1200, height=800, res=96, antialias = 'cleartype')

trellis.par.set(list(superpose.polygon=list(col=ms.colors)))
sK <- simpleKey(text = c('very dry', 'dry', 'moist', 'wet', 'saturated', 'ponded'), space='top', columns=3, rectangles = TRUE, points=FALSE, cex=0.8)

levelplot(state.stack[[idx]], main='Estimated Moisture State',
          margin=FALSE, scales=list(draw=FALSE),
          colorkey=FALSE,
          key=sK,
          at=0:6, col.regions=ms.colors,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(contours, col='black', lwd=1)
          }
)

dev.off()


dd <- data.frame(
  PPT=dm$prcp..mm.day.[idx],
  PET=as.vector(ET$ET.Daily[idx])
)

knitr::kable(dd, digits = 2)

# test at farm center... hey!
e <- extract(VWC.stack, pt)
plot(as.vector(e), type='l')

e <- sampleRegular(VWC.stack, 40)
matplot(t(e), type='l', col='grey', lty=1)



animate(state.stack[[1:10]], zlim=c(0.5, 6.5), col=ms.colors, pause=0.05, n=1)
animate(VWC.stack[[1:230]], zlim=c(0.2, 0.4), col=viridis(30), pause=0.05, n=1)


