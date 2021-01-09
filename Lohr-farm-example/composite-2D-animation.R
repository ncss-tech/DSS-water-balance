library(raster)
library(viridis)
library(rgdal)
library(raster)
# library(scico)
library(reshape2)
library(lattice)
library(rasterVis)
library(progress)

## moisture state colors, still thinking about these
ms.colors <- brewer.pal(6, 'Spectral')

elev <- raster('DEM-and-derived/elev_3m_utm.tif')
ssurgo <- readOGR(dsn='.', layer='ssurgo_map', stringsAsFactors = FALSE)

# mask elev for contours
ssurgo$mask <- 1
ssurgo.rast <- rasterize(ssurgo, elev, field='mask')

elev <- mask(elev, ssurgo.rast)
contours <- rasterToContour(elev, nlevels=10)

plot(elev, axes=FALSE)
lines(contours)

VWC <- stack('results/VWC_time-series.grd')
VWC <- readAll(VWC)

# these are created in gridded-WB.R
PPT <- stack('results/distributed-PPT_time-series.grd')
PET <- stack('results/distributed-PET_time-series.grd')
PPT <- readAll(PPT)
PET <- readAll(PET)

# integer coding of factor levels
moisture.state <- stack('results/moisture_state_time-series.grd')
moisture.state <- readAll(moisture.state)

# ## hack when NODATA not set and used by default
# NAvalue(moisture.state) <- 0
# writeRaster(moisture.state, file='results/moisture_state_time-series.grd', overwrite=TRUE, NAflag=0)

## check
s <- seq(0.16, 0.4, by=0.02)
cols <- rev(viridis(length(s)))

# OK
par(mar=c(1,1,3,1))
plot(VWC[[100]], axes=FALSE, breaks=s, col=cols)
lines(contours, col='white')


## extract / convert dats from gridded WB output layer names
dates <- gsub('X', '', names(moisture.state))
dates <- as.Date(dates, format = "%Y.%m.%d")


## think carefully about where to put this point...
ex.pt <- SpatialPoints(cbind(695000, 4270800), proj4string = CRS(projection(moisture.state)))
ex.pt.VWC <- as.vector(extract(VWC, ex.pt))
ex.pt.PPT <- as.vector(extract(PPT, ex.pt))
ex.pt.df <- data.frame(date=dates, VWC=ex.pt.VWC, PPT=ex.pt.PPT)
ex.pt.df <- melt(ex.pt.df, id.vars = 'date')
ex.pt.df$variable <- factor(ex.pt.df$variable, levels=c('VWC', 'PPT'), labels = c('Volumetric Water Content', 'Daily Precipitation (mm)'))

## make a legend for the PPT
png(file='figures/moisture-state-legend.png', width=400, height=375)
par(mar=c(0, 0, 0, 0))
plot(1, 1, type='n', axes=FALSE, xlab='', ylab='')
legend(
  'topleft', legend=c('very dry', 'dry', 'moist', 'very moist', 'wet', 'saturated'), 
  col=ms.colors, pch=15, cex=5, pt.cex=8, bty='n',
  y.intersp=0.75
)
dev.off()

## colors for GRASS r.colors
# 1 -> 6
cat(apply(t(col2rgb(ms.colors)), 1, paste, collapse=':'), sep='\n')


## TODO: series viz / series-specific VWC thresholds 
# simple.ssurgo <- raster('results/simplified_ssurgo_single_series_hyd_data.grd')
# extract(simple.ssurgo, ex.pt)

## tigher limits on x-axis
ppt.xlim <- c(min(ex.pt.df$date) - 10, max(ex.pt.df$date) + 10)


n.days <- nlayers(moisture.state)
pb <- progress_bar$new(total = n.days)

for(idx in 1:n.days) {
  
  this.date <- dates[idx]
  fname <- sprintf("E:/temp/2D/moisture-state-%03d.png", idx)
  
  trellis.par.set(list(superpose.polygon=list(col=ms.colors)))
  sK <- simpleKey(text = c('very dry', 'dry', 'moist', 'very moist', 'wet', 'saturated'), space='top', columns=3, rectangles = TRUE, points=FALSE, cex=1)
  
  p.1 <- levelplot(moisture.state[[idx]], main=sprintf('Estimated Moisture State: %s', format(this.date, "%b %d %Y")),
                   margin=FALSE, scales=list(draw=FALSE),
                   colorkey=FALSE,
                   key=sK,
                   at=0:6, col.regions=ms.colors,
                   par.settings=tactile::tactile.theme(),
                   panel=function(...) {
                     panel.levelplot(...)
                     sp.polygons(contours, col='black', lwd=1)
                     sp.points(ex.pt, pch=0, col='black', lwd=3, cex=1.5)
                   }
  )
  
  
  p.2 <- xyplot(value ~ date | variable, data=ex.pt.df, 
                type=c('S', 'g'), ylab='', xlab='', 
                xlim=ppt.xlim,
                # scales=list(x=list(format="%b %d\n%Y"), y=list(relation='free', rot=0, tick.number=2), cex=1), 
                scales=list(x=list(format="%b %d\n%Y", cex=1.125), y=list(relation='free', draw=FALSE)), 
                strip=strip.custom(bg=grey(0.9)),
                par.strip.text=list(cex=1.25),
                par.settings=tactile::tactile.theme(plot.line=list(col=grey(0.4), lwd=1)),
                layout=c(1,2), 
                panel=function(x, y, ...) {
                  x.now <- x[x <= this.date]
                  y.now <- y[x <= this.date]
                  panel.xyplot(x=x, y=y, ...)
                  panel.lines(x=x.now, y=y.now, col='royalblue', type='S', lwd=1)
                  panel.points(x=x.now[length(x.now)], y=y.now[length(y.now)], pch=15, col='firebrick', cex=1)
                }
  )
  
  
  # ad-hoc re-labeling of panels
  row.names(p.2) <- c('Soil Moisture', 'Precipitation')
  
  ## composite figure for animation
  png(file=fname, width=950, height=1200, res=100)
  
  print(p.1, position=c(0, 0.33, 1, 1), more=TRUE)
  print(p.2, position=c(0, 0, 1, 0.33))
  
  dev.off()
  
  ## PPT/VWC figure for appending to 3D renders
  ppt.fname <- sprintf("E:/temp/PPT/time-series-%03d.png", idx)
  png(file=ppt.fname, width=1000, height=450, res=100)
  print(p.2)
  dev.off()
  
  pb$tick()
}




