## main animations
convert -delay 16 -loop 0  2D/*.png +dither +map moisture-state-animation.gif &
convert -delay 16 -loop 0  r.sim.water-render/*.png +dither +map -scale 900x water_depth_3D.gif &
convert -delay 16 -loop 0  moisture-status-render/*.png +dither +map -scale 900x moisture-state_3D.gif &

## secondary applications
# convert -delay 16 -loop 0 water_depth_2D/*.png +dither +map water_depth_2D.gif &
# convert -delay 10 -loop 0  VWC_frames/*.png -scale 500x VWC.gif
# convert -delay 16 -loop 0  VWC-render/*.png +dither +map -scale 500x VWC_3D.gif & 
# convert -delay 16 -loop 0  U-render/*.png +dither +map -scale 500x U_3D.gif & 


## composite images
# https://superuser.com/questions/290656/combine-multiple-images-using-imagemagick
# convert -append moisture-status-render/moisture_state.0002.png PPT/time-series-002.png -scale 500x out.png

seq 1 365 | parallel --gnu --eta --bar "./make-composite-frame.sh {}"



### TODO: consider partial optimization of color tables at animation step

# result is ~ 40Mb
# reduce colors without dithering: need at least 128 `-colors 128` (27Mb)
# `+map` will attempt global optimization (34Mb)
convert -delay 16 -loop 0 composite/*.png +dither +map composite-animation.gif

## optimize
# https://www.lcdf.org/gifsicle/
# http://www.imagemagick.org/Usage/quantize/#remap
# http://www.imagemagick.org/Usage/anim_opt/#colortables

# compress: 10-20x !!!
gifsicle -O3 moisture-state-animation.gif -o moisture-state-animation-opt.gif
gifsicle -O3 water_depth_3D.gif -o water_depth_3D-opt.gif
gifsicle -O3 moisture-state_3D.gif -o moisture-state_3D-opt.gif
gifsicle -O3 composite-animation.gif -o composite-animation-opt.gif






