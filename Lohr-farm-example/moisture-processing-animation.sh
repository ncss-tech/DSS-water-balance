## using data from gridded water balance

## this works at home, not on basho
# gdal_translate results/vwc.grd vwc.tif

## convert in R on basho

# import multi-band images
r.in.gdal -o in=vwc.tif out=vwc num_digits=4 --o
r.in.gdal -o in=U.tif out=U num_digits=4 --o

# be sure to export from R as integer
r.in.gdal -o in=moisture-state.tif out=moisture_state num_digits=4 --o

# clean-up required for categorical maps
# setting NULL
# establishing categories
r=$(g.list rast pattern=moisture_state.???? sep=" ")
for i in $r
do

r.null map=$i setnull=0

r.category $i separator=":" rules=- << EOF
1:very dry
2:dry
3:moist
4:wet
5:saturated
6:ponded
EOF

done


# set consistent color ramps
r=$(g.list rast pattern=vwc.???? sep=",")
r.colors --q -e -n map=$r color=viridis

r=$(g.list rast pattern=U.???? sep=",")
r.colors --q -e -n map=$r color=viridis

r=$(g.list rast pattern=moisture_state.???? sep=",")
r.colors --q map=$r rules=moisture-state-colors.rules



## animate
export GRASS_RENDER_TRANSPARENT=TRUE

# VWC
r=$(g.list rast pattern=vwc.???? sep=" ")
for i in $r
do
f="VWC_frames/${i}.png"
d.mon --o start=cairo output=$f width=6 height=5 resolution=96
d.rast $i
d.mon stop=cairo
done

# U
r=$(g.list rast pattern=U.???? sep=" ")
for i in $r
do
f="U_frames/${i}.png"
d.mon --o start=cairo output=$f width=6 height=5 resolution=96
d.rast $i
d.mon stop=cairo
done

## note: special care requried for ignoring NODATA
## bgcolor=none
## values=1-6
# moisture state
r=$(g.list rast pattern=moisture_state.???? sep=" ")
for i in $r
do
f="moisture-status_frames/${i}.png"
d.mon --o start=cairo output=$f width=6 height=5 resolution=96 bgcolor=none
d.rast $i values=1-6
d.mon stop=cairo
done
