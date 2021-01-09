## Get essential GIS data for Lohr Family farm
## 2020-01-09
## D.E. Beaudette

## output saved in EXTRACTIONS

## Notes:
# LiDAR processed separately--not working due to CRS problems

## TODO: clean-up and remove stuff we aren't using


library(rgdal)
library(FedData)
library(mapview)
library(daymetr)
library(aqp)
library(soilDB)
library(sharpshootR)
library(raster)
library(rasterVis)
library(reshape2)
library(viridis)

## 2011 NLCD
r <- raster('E:/gis_data/NLCD/nlcd_2011_landcover_2011_edition_2014_03_31.img')
farm <- readOGR(dsn='LohrFarm', layer='LohrFarm', stringsAsFactors = FALSE)

farm <- spTransform(farm, CRS(projection(r)))

lohr_farm_nlcd <- crop(r, farm)

plot(lohr_farm_nlcd)
plot(farm, add=TRUE)

writeRaster(lohr_farm_nlcd, filename = 'E:/temp/lohr_farm_nlcd.tif')

##


# GCS: WGS84
# farm outline
farm <- readOGR(dsn='LohrFarm', layer='LohrFarm', stringsAsFactors = FALSE)

# center of farm for climate data queries
pt <- rgeos::gCentroid(farm)

# check: OK
mapview(farm)


## 1/3 arc-second (10m) DEM
# ~ 15 minutes to download
# https://docs.ropensci.org/FedData/reference/get_ned.html
elev <- get_ned(template=polygon_from_extent(farm), label = 'lohr_farm', res='13')

# check: OK
plot(elev)
plot(farm, add=TRUE)
points(pt)


## 1/9 arc-second (3m) DEM
# some artifacts from buildings / ponds--10m has same issues
# crop to buffered farm bbox and warp to utm z17
# of course this is better done by gdalwarp

# c/o Suzann
# buffered and UTM 
s <- readOGR('.', layer='ssurgo_map', stringsAsFactors = FALSE)
s.gcs <- spTransform(s, CRS(projection(r1)))

# tiles of interest
r1 <- raster('E:/gis_data/VA/ned19_n38x75_w078x75_va_nrcs_lot2_2011.img')
r2 <- raster('E:/gis_data/VA/ned19_n38x75_w079x00_va_nrcs_lot2_2011.img')
elev_3m <- mosaic(r1, r2, fun=mean)
elev_3m <- crop(elev_3m, s.gcs)

# check: OK
plot(elev_3m)
plot(s.gcs, add=TRUE)

## warp and save
elev_utm <- projectRaster(from=elev_3m, res=3, crs=CRS(proj4string(s)), method = 'bilinear')

# check: OK
plot(elev_utm)
plot(s, add=TRUE)

# 30m version for TWI
# not likely useful -- too blocky
e30 <- aggregate(elev_utm, fact=10)


## save
writeRaster(elev_utm, filename = 'DEM-and-derived/elev_3m_utm.tif')
writeRaster(e30, filename = 'DEM-and-derived/elev_3m_utm_30m_mean.tif', overwrite=TRUE)



## annual beam radiance -> distributed ET
abr <- raster('DEM-and-derived/beam_rad_3m.tif')
abr <- readAll(abr)

levelplot(abr, main='Annual Beam Radiance (3m)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

# decimate for animation (?)
abr.approx <- aggregate(abr, fact=9)
normalized.abr <-  abr.approx / median(abr.approx[], na.rm=TRUE)

# check: this will work for animation
levelplot(normalized.abr, main='Normalized Annual Beam Radiance (~30m)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

writeRaster(normalized.abr, filename = 'DEM-and-derived/normalized_abr.tif', overwrite=TRUE)



## Saga TWI -> distributed PPT
twi <- raster('DEM-and-derived/saga_twi_3m.tif')
twi <- readAll(twi)

levelplot(twi, main='SAGA Wetness Index (3m)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

# decimate for animation (?)
twi.approx <- aggregate(twi, fact=9)
normalized.twi <-  twi.approx / median(twi.approx[], na.rm=TRUE)

# check: this will work for animation
levelplot(normalized.twi, main='Normalized SAGA Wetness Index (~30m)',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

writeRaster(normalized.twi, filename = 'DEM-and-derived/normalized_saga_twi.tif', overwrite=TRUE)


## rasterize SSURGO
s$MUKEY <- as.integer(s$MUKEY)
ssurgo.approx <- rasterize(s, twi.approx, field='MUKEY')

# check: OK
levelplot(ssurgo.approx, margin=FALSE, scales=list(draw=FALSE))

# add RAT then merge in useful data
ssurgo.approx <- ratify(ssurgo.approx)
rat <- levels(ssurgo.approx)[[1]]

ws <- sprintf("mukey IN %s AND compkind != 'Miscellaneous area'", format_SQL_in_statement(rat$ID))

# include mukey
x <- fetchSDA_component(WHERE = ws, duplicates = TRUE)

par(mar=c(0, 0, 3, 1))
plot(x, label='compname', color='awc_r')

# wide format for selecting component names in aggregation to 1:1 with mukey
mukey.comp.LUT <- dcast(site(x), mukey ~ compname, value.var='comppct_r', fill = 0)
# mukey.cokey.LUT <- dcast(site(x), mukey ~ cokey, value.var='comppct_r', fill = 0)


# aggregate series names by sorting comp pct, combine complexes into new labels
compnames <- apply(mukey.comp.LUT[, -1], 1, function(i) {
  i <- sort(i[i > 0], decreasing = TRUE)
  paste(names(i), collapse='/')
})

# single series, no tie-breaking rules or other smart stuff
compnames.simple <- apply(mukey.comp.LUT[, -1], 1, function(i) {
  idx <- sort(i[i > 0], decreasing = TRUE)[1]
  names(idx)
})

# single cokey
cokeys <- apply(mukey.comp.LUT[, -1], 1, function(i) {
  i <- sort(i[i > 0], decreasing = TRUE)
  paste(names(i), collapse='/')
})

# pack into RAT
rat$comp <- factor(compnames)
rat$series <- factor(compnames.simple)
rat$mukey <- rat$ID
# put RAT into raster
levels(ssurgo.approx)[[1]] <- rat

# check: OK
levelplot(ssurgo.approx, att='mukey', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='black', lwd=2)
          }
)

writeRaster(ssurgo.approx, filename='results/simplified_ssurgo_with_rat.grd', format='raster', overwrite=TRUE)

# convert into copmlex / series rasters
ssurgo_simple.approx <- deratify(ssurgo.approx, att='series')
ssurgo_complexes.approx <- deratify(ssurgo.approx, att='comp')

# check: OK
levelplot(ssurgo_complexes.approx, att='comp', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=brewer.pal(8, 'Set1'),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

# OK
levelplot(ssurgo_simple.approx, att='series', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=brewer.pal(5, 'Set1'),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='white', lwd=1)
          }
)

writeRaster(ssurgo_complexes.approx, filename='results/simplified_ssurgo_complexes.grd', format='raster', overwrite=TRUE)
writeRaster(ssurgo_simple.approx, filename='results/simplified_ssurgo_single_series.grd', format='raster', overwrite=TRUE)

## quick viz of OSDs
series.names <- unique(x$compname)
osd <- fetchOSD(series.names, extended = TRUE)


SoilTaxonomyDendrogram(osd$SPC, width=0.3, cex.taxon.labels = 0.8)


hs <- vizHillslopePosition(osd$hillpos)
plot(osd$SPC, width=0.3, plot.order=hs$order)

gc <- vizGeomorphicComponent(osd$geomcomp)
plot(osd$SPC, width=0.3, plot.order=gc$order)

vizAnnualClimate(osd$climate.annual)




### not using these because we don't need the full tiles and would take many hours
# ## DAYMET
# # ~ 5 min per variable / year
# # consider using single-pixel API: https://daymet.ornl.gov/single-pixel/
# # https://docs.ropensci.org/FedData/reference/get_daymet.html
# daytmet <- get_daymet(template=polygon_from_extent(farm),
#                      label='lohr_farm',
#                      elements = c('prcp', 'tmin', 'tmax', 'dayl'),
#                      years = 2016)
# 
# # check: OK
# plot(daymet$tmin$X2016.10.23)
# plot(farm, add=TRUE, lwd=1)

# ## Daymet at center of farm
# daymet <- download_daymet("lohr_farm",
#                           lat = coordinates(pt)[, 2],
#                           lon = coordinates(pt)[, 1],
#                           start = 1981,
#                           end = 2010,
#                           internal = TRUE)
# 
# 


# none close by--try expanding 
# ## GHCN / local WX station
# GHCN.prcp <- get_ghcn_daily(template=polygon_from_extent(farm), label='lohr_farm', elements=c('prcp', 'tmin', 'tmax'))
# 
# # check:


## SSURGO
# often fails on the first try
ssurgo <- get_ssurgo(template=polygon_from_extent(farm), label='lohr_farm')

# check: OK
plot(ssurgo$spatial)
plot(farm, add=TRUE, border='red', lwd=2)


## SDA
ws <- sprintf("mukey IN %s AND compkind != 'Miscellaneous area'", format_SQL_in_statement(unique(ssurgo$spatial$MUKEY)))
x <- fetchSDA_component(WHERE = ws)

# duplication of copmonents
plot(x, label='compname', color='awc_r')
plot(x, label='compname', color='awc_r')

s <- unique(x$compname)
osd <- fetchOSD(s, extended = TRUE)

SoilTaxonomyDendrogram(osd$SPC, width=0.3, cex.taxon.labels = 0.8)


vizHillslopePosition(osd$hillpos)
vizGeomorphicComponent(osd$geomcomp)
vizAnnualClimate(osd$climate.annual)

## PRISM stack


## gemorphons


## NLCD


## optionally sample daily PRISM stack on basho




