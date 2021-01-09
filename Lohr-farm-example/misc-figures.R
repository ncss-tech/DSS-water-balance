library(aqp)
library(soilDB)
library(sharpshootR)
library(knitr)
library(rasterVis)
library(rgdal)
library(sp)
library(reshape2)


source('local-functions.R')

## local SSURGO map
# c/o Suzann
# buffered and UTM 
ssurgo <- readOGR('.', layer='ssurgo_map', stringsAsFactors = FALSE)

## simplified / aggregated SSURGO
ss <- raster('results/simplified_ssurgo_single_series.grd')

# water retention data
load('cached-data/ssurgo-component-data.rda')

# local copy
x <- s$agg
# add fields expected by plotAvailWater()
x$solid <- with(x, 1 - sat)
x$name <- x$compname

## relative differences... AWC values aren't qite right
png(file='figures/water-fractions.png', width=800, height=400, antialias = 'cleartype', res=90)
par(mar=c(4.5, 7, 1, 1))
plotAvailWater(x, width = 0.33, name.cex = 1.5, annotate = FALSE)
dev.off()


## OSDs for misc. figures
osd <- fetchOSD(s$agg$compname, extended = TRUE)

# OSD figure
png(file='figures/OSD-dend.png', width=800, height=650, antialias = 'cleartype', res=90)
SoilTaxonomyDendrogram(osd$SPC, width=0.3, cex.taxon.labels = 1, cex.names=1.125, cex.id=1)
dev.off()



## simplified SSURGO figure
p.1 <- levelplot(ss, att='series',
          main='Simplified SSURGO\n~100x100 ft. grid',
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=brewer.pal(5, 'Set1'),
          colorkey=list(space='right', labels=list(cex=2)),
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(ssurgo, col='black', lwd=2)
          }
)

png(file='figures/simlified-ssurgo-series.png', width=900, height=650, antialias = 'cleartype', res=90)
print(p.1)
dev.off()


## make a fake component map

# add Lodi to the mix
cokeys <- c('17469321', '17469317', '17469215', '17469245', '17469354', '17469214')
z <- getSSURGO_hydro_data(cokeys = cokeys, max.depth = 100)

osd <- fetchOSD(z$agg$compname, extended = TRUE)

hs <- vizHillslopePosition(osd$hillpos)
gc <- vizGeomorphicComponent(osd$geomcomp)

par(mar=c(0,0,0,0))
plotSPC(osd$SPC, plot.depth.axis = FALSE, cex.names=1, width=0.3)
