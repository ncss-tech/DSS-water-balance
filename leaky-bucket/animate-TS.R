##
## this isn't going to work
##


library(raster)
library(viridis)
library(rgdal)
library(magick)
library(raster)
library(scico)

elev <- raster('elev_3m_utm.tif')
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

moisture.state <- stack('results/moisture_state_time-series.grd')
moisture.state <- readAll(moisture.state)

s <- seq(0.16, 0.4, by=0.02)
cols <- rev(viridis(length(s)))

par(mar=c(1,1,3,1))
plot(VWC[[100]], axes=FALSE, breaks=s, col=cols)
lines(contours)

idx <- seq(from=1, to=365, by=5)

# ## very slow
# ## doesn't scale beyond ~ 100 days?
# # https://cran.r-project.org/web/packages/magick/vignettes/intro.html#animated_graphics
# # capture device output
# fig <- image_graph(width = 600, height = 500, res = 96)
# 
# par(mar=c(1,1,3,1))
# for(i in idx) {
#   nm <- names(VWC[[i]])
#   nm <- gsub('X', '', nm)
#   plot(VWC[[i]], axes=FALSE, breaks=s, col=cols, main=sprintf("VWC: %s", nm))
#   lines(contours)
# }
# 
# dev.off()
# 
# an <- image_animate(fig, fps = 10, dispose = "previous")
# 
# ## will this ever finish? not likely
# # will have to do this via GRASS and d.his
# image_write(an, 'e:/temp/VWC-animation.gif')
# 
# 


# ##
# ## moisture state
# ms.colors <- rev(scico(6, palette='vik'))
# 
# fig <- image_graph(width = 600, height = 500, res = 96)
# 
# par(mar=c(1,1,3,0))
# for(i in idx) {
#   nm <- names(moisture.state[[i]])
#   nm <- gsub('X', '', nm)
#   plot(moisture.state[[i]], axes=FALSE, legend=FALSE, breaks=1:6, col=ms.colors, main=sprintf("Moisture State: %s", nm))
#   lines(contours)
#   legend('bottomright', legend = c('very dry', 'dry', 'moist', 'wet', 'saturated', 'flooded'), pch=15, col=ms.colors, bty='n', pt.cex=1.5, cex=0.85)
# }
# 
# dev.off()
# 
# an <- image_animate(fig, fps = 10, dispose = "previous")
# 
# ## will this ever finish? not likely
# # will have to do this via GRASS and d.his
# image_write(an, 'e:/temp/moisture-state-animation.gif')
# 

