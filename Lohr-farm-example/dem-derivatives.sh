##
##
##

## created a new location from file
# r.in.gdal input=elev_3m_utm.tif out=elev3m location=lohr_farm

##
## r.sim.water results need some tweaking before we can use this to distribute PPT
## normalized by median results in many cells == 0
## maybe this can be used via: PPT + (PPT * normalized simulated runoff)
##

## ssurgo
v.in.ogr --o input=data/ssurgo_map.shp output=ssurgo

g.region rast=elev3m -ap

# hill shade
r.relief --o input=elev3m output=shade3m

# contours
r.contour --o -t input=elev3m output=contours step=5

# geomorphons
r.geomorphon --o -e elevation=elev3m forms=forms3m search=30	
# cleaning... better performed at DEM smoothing step
# 0.40469 ha ~ 1 ac.
r.reclass.area --o -d input=forms3m output=forms_clean value=0.40469 mode=lesser method=rmarea
r.colors forms_clean rast=forms3m

## this works great, about 1 minute to run
## consider less detail
# partial derivatives
r.slope.aspect --o elevation=elev3m dx=elev3m_dx dy=elev3m_dy aspect=elev3m_aspect slope=elev3m_slope

# water simulation
# https://grass.osgeo.org/grass78/manuals/r.sim.water.html
r.sim.water --o elevation=elev3m dx=elev3m_dx dy=elev3m_dy \
depth=water_depth discharge=discharge \
nprocs=8 \
rain_value=50 infil_value=0.1

# time series
# 60 minutes of simulation
# 1" of rain
r.sim.water --o -t elevation=elev3m dx=elev3m_dx dy=elev3m_dy \
depth=water_depth discharge=discharge \
nprocs=8 output_step=1 \
rain_value=25.4 infil_value=0.2 niterations=60 hmax=0.1

# maps
r=$(g.list type=rast pattern=water_depth.??)
export GRASS_RENDER_TRANSPARENT=TRUE

# 2D animation frames
for i in $r
do
f="water_depth_2D/${i}.png"


d.mon --o start=cairo output=$f width=6 height=5 resolution=96
d.his h=$i i=shade3m brighten=25
d.vect ssurgo fill=none color=grey
d.mon stop=cairo

done

# 3D animation frames
for i in $r
do
f="water_depth_3D/${i}.png"

d.mon --o start=cairo output=$f width=6 height=5 resolution=96
# suppress the smallest values
d.rast $i values=0.005-999
d.mon stop=cairo

done



## see animation-notes.sh for animation parameters


# flow lines and acc
r.flow --o -3 elevation=elev3m aspect=elev3m_aspect flowline=flowlines3m flowaccumulation=flowacc3m skip=5

# annual beam radiance
r.sun.daily --o elevation=elev3m aspect=elev3m_aspect slope=elev3m_slope \
start_day=1 end_day=365 nprocs=8 \
beam_rad=beam3m

# export
r.out.gdal --o -c input=beam3m out=beam_rad_3m.tif createopt=COMPRESS=LZW


## smooth via RST
# this has problems when region doesn't align correctly... why? res=30 for example
r.resamp.rst --o -t input=elev3m ew_res=15 ns_res=15 elevation=elev_approx

g.region rast=elev_approx -ap
r.slope.aspect --o elevation=elev_approx dx=elev_approx_dx dy=elev_approx_dy aspect=elev_approx_aspect

# looks good
r.geomorphon --o -e elevation=elev_approx forms=forms_approx search=30	
r.reclass.area -d --o input=forms_approx output=forms_approx_clean mode=lesser value=0.1 method=rmarea
r.colors map=forms_approx_clean rast=forms_approx

## save a static copy for 3D viz
d.mon --o start=cairo output=forms_approx_clean.png width=6 height=5 resolution=96
d.rast forms_approx_clean
d.mon stop=cairo



# water simulation
r.sim.water --o elevation=elev_approx dx=elev_approx_dx dy=elev_approx_dy depth=water_depth_approx nprocs=4 rain_value=50

# normalize via median
r.univar -e water_depth_approx
r.mapcalc --o "water_depth_approx_norm = water_depth_approx / 0.0106945"

# flow lines and acc
r.flow --o -3 elevation=elev_approx aspect=elev_approx_aspect flowline=flowlines flowaccumulation=flowacc





