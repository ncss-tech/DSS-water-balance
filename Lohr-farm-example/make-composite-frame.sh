#!/bin/bash

i=$1

# indexes to slightly different file name conventions
graph_idx=$(printf "%03d" $i)
render_idx=$(printf "%04d" $i)

# file names
graph_fname="PPT/time-series-${graph_idx}.png"
render_fname="moisture-status-render/moisture_state.${render_idx}.png"

# output file name
comp_fname="composite/comp-${graph_idx}.png"

# append after fixed-aspect scaling by width
convert -append $render_fname $graph_fname -scale 900x $comp_fname

