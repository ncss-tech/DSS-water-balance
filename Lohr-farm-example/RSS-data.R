##
## not using this
##


library(raster)
library(rasterVis)
library(rgdal)
library(foreign)
library(viridis)
library(colorspace)
library(aqp)
library(soilDB)
library(reshape2)
library(sharpshootR)

r <- raster::raster('E:/temp/lohr_rss.tif')
rat <- foreign::read.dbf('E:/temp/lohr_rss.tif.vat.dbf')

names(rat)[1] <- 'ID'
levels(r)[[1]] <- rat

s <- readOGR('e:/temp', layer='ssurgo_map')

cols <- diverge_hcl(nrow(rat), palette = "Cork") 

levelplot(r, attr='MUSYM', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=cols,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='black', lwd=2)
          }
)


ws <- sprintf("mukey IN %s AND compkind != 'Miscellaneous area'", format_SQL_in_statement(rat$MUKEY))

# include mukey
x <- fetchSDA_component(WHERE = ws, duplicates = TRUE)

par(mar=c(0, 0, 3, 1))
plot(x, label='compname', color='awc_r')


mukey.comp.LUT <- dcast(site(x), mukey ~ compname, value.var='comppct_r', fill = 0)


compnames <- apply(mukey.comp.LUT[, -1], 1, function(i) {
  i <- sort(i[i > 0], decreasing = TRUE)
  paste(names(i), collapse='/')
})

rat$comp <- factor(compnames)
levels(r)[[1]] <- rat

levelplot(r, att='MUSYM', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=cols,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='black', lwd=2)
          }
)

writeRaster(r, filename='rss_with_rat.grd', format='raster', overwrite=TRUE)


rc <- deratify(r, att='comp')

levelplot(rc, att='comp', 
          margin=FALSE, scales=list(draw=FALSE),
          col.regions=viridis,
          panel=function(...) {
            panel.levelplot(...)
            sp.polygons(s, col='black', lwd=2)
          }
)






