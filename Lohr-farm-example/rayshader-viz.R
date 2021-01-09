
## Notes:

# https://www.rayshader.com/

# https://github.com/tylermorganwall/rayshader
# https://github.com/tylermorganwall/rayshader/issues/30
# https://github.com/tylermorganwall/rayshader/issues/25

library(rayshader)
library(raster)
library(magick)
library(progress)
library(sp)

# elevation for a chunk of space
# export form ArcMap:
# Data -> Export Data
# Extent: "Data Frame"
# save to GeoTiff with LZW compression
elev <- raster('DEM-and-derived/elev_3m_utm.tif')
elev <- elev * 3


## annotation
# not using this
ex.pt <- SpatialPoints(cbind(695000, 4270800), proj4string = CRS(projection(elev)))
pt.cell <- extract(elev, ex.pt, cellnumbers=TRUE)
pt.row.col <- as.vector(rowColFromCell(elev, pt.cell[, 1]))

## double-check this...
# rayshader uses transposed indexing
# rayshader column = raster row
ray.col <- pt.row.col[1]
# rayshader row = ncol - raster column
ray.row <- ncol(elev) - pt.row.col[2]

# https://wcmbishop.github.io/rayshader-demo/

## TODO
# * make 2D VWC, U, moisture state thematic maps in GRASS and animate
# load un-scaled overlay
# this has to be a thematic map BTW
ov <- png::readPNG('figures/forms_approx_clean.png')

# convert raster -> matrix
elmat <- matrix(extract(elev, extent(elev), buffer=1000), nrow=ncol(elev), ncol=nrow(elev))


# compute shadows
raymat <- ray_shade(elmat)
ambmat <- ambient_shade(elmat)


## testing 
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  add_overlay(ov, alphalayer = 0.8) %>%
  plot_map()


## reasonable camera parameters
## theta (z-axis rotation) ~ 25
## phi (azimuth) ~ 30

## better camera params
.theta <- 15
.phi <- 35
.fov <- 0

## output size
px.width <- 1200
px.height <- 800


## geomorphons example
elmat %>%
  sphere_shade(texture = "imhof4") %>%
  add_shadow(raymat) %>%
  add_shadow(ambmat) %>%
  add_overlay(ov, alphalayer = 0.6) %>%
  plot_3d(elmat, zscale = 2, windowsize = c(px.width, px.height),
          baseshape='rectangle', lineantialias=TRUE,
          theta = .theta, phi = .phi, zoom = 0.45, fov = .fov)

render_snapshot(filename = 'figures/forms-3D.png', clear=TRUE, vignette=FALSE, instant_capture = TRUE)

rgl::rgl.close()


## annotation
# elmat %>%
#   sphere_shade(texture = "imhof4") %>%
#   add_shadow(raymat) %>%
#   add_shadow(ambmat) %>%
#   add_overlay(ov, alphalayer = 0.6) %>%
#   plot_3d(elmat, zscale = 2, windowsize = c(1200, 800),
#           theta = 15, phi = 35, zoom = 0.45, fov = 0)
# 
# render_label(elmat, x = ray.col, y = ray.row, z = 100, zscale = 2,
#              text = "Frederick", textsize = 1, linewidth = 5, freetype = FALSE)
# 
# rgl::rgl.close()


##
## 3D moisture state
##
p.input <- 'e:/temp/moisture-status_frames'
p.output <- 'e:/temp/moisture-status-render'

f <- list.files(p.input, pattern = '*.png$')
n <- length(f)
pb <- progress_bar$new(total = n)

for(i in seq_along(f)) {
# for(i in floor(seq(1, 365, length.out=10))) {
  
  f.i <- f[i]
  of <- file.path(p.output, f.i)
  
  ov <- png::readPNG(file.path(p.input, f.i))
  time.i <- as.numeric(strsplit(f.i, '.', fixed=TRUE)[[1]][2])
  # tt <- sprintf("1 in. rain event: %s minutes", time.i)
  
  # calendar days
  time.i <- format(as.Date(sprintf("2015 %s", time.i), format = "%Y %j"), "%b %d, %Y")
  tt <- sprintf("Soil Moisture Status: %s", time.i)

  elmat %>%
    sphere_shade(texture = "imhof4") %>%
    add_shadow(raymat) %>%
    add_shadow(ambmat) %>%
    add_overlay(ov, alphalayer = 0.6) %>%
    plot_3d(elmat, zscale = 2, windowsize = c(px.width, px.height),
            baseshape='rectangle', lineantialias=TRUE,
            theta = .theta, phi = .phi, zoom = 0.45, fov = .fov)
  
  ## too hard to see  
  # render_label(elmat, x = ray.col, y = ray.row, z = 100, zscale = 2,
  #              text = "Frederick", textsize = 1, linewidth = 5, freetype = FALSE)
  
  render_snapshot(
    filename = of, clear=TRUE, 
    vignette=FALSE, instant_capture = TRUE, 
    title_color='black', title_offset=c(10, 10), title_size=25, title_bar_color=grey(0.5), title_text=tt,
    gravity='northeast', weight=700
  )
  
  # progress bar
  pb$tick()
}

## do this after a session, to clear rgl device
rgl::rgl.close()




##
## 3D r.sim output
##
p.input <- 'e:/temp/r.sim.water-frames'
p.output <- 'e:/temp/r.sim.water-render'

f <- list.files(p.input, pattern = '*.png$')
n <- length(f)
pb <- progress_bar$new(total = n)

for(i in seq_along(f)) {
  
  f.i <- f[i]
  of <- file.path(p.output, f.i)
  
  ov <- png::readPNG(file.path(p.input, f.i))
  
  ## annotation
  time.i <- as.numeric(strsplit(f.i, '.', fixed=TRUE)[[1]][2])
  tt <- sprintf("1 in. rain event: %s minutes", time.i)
  
  elmat %>%
    sphere_shade(texture = "imhof4") %>%
    add_shadow(raymat) %>%
    add_shadow(ambmat) %>%
    add_overlay(ov, alphalayer = 0.6) %>%
    plot_3d(elmat, zscale = 2, windowsize = c(px.width, px.height),
            baseshape='rectangle', lineantialias=TRUE,
            theta = .theta, phi = .phi, zoom = 0.45, fov = .fov)
  
  
  render_snapshot(
    filename = of, clear=TRUE, 
    vignette=FALSE, instant_capture = TRUE, 
    title_color='black', title_offset=c(10, 10), title_size=25, title_bar_color=grey(0.5), title_text=tt,
    gravity='northeast', weight=700
  )
  
  # progress bar
  pb$tick()
}

## do this after a session, to clear rgl device
rgl::rgl.close()





## TODO for next time
# * move camera during animation

# move camera
# render_camera(theta = 25, phi = 30, zoom = 0.45, fov = 45)

## too hard to see when Freetype library missing
# render_label(elmat, x = ray.col, y = ray.row, z = 100, zscale = 2,
#              text = "Frederick", textsize = 1, linewidth = 5, freetype = FALSE)

