

## clean this up
## assemble GIS data from pieces
# source('assemble-GIS-data.R')

## RSS map
# source('RSS-data.R')

## climate data 1988--2018
source('get-daily-climate-data.R')

## ssurgo component hyd data
source('get-ssurgo-comp-data.R')

## misc. figures
source('misc-figures.R')

## daily WB by series 
source('prepare-daily-wb-by-series.R')

## monthly / weekly wb summaries
source('aggregate-moisture-state-figures.R')

## develop gridded soil hydraulic data
source('develop-series-hyd-data.R')

## gridded WB stack
source('gridded-WB.R')

## 2D animation of WB stacks
# source('animate-TS.R')
source('composite-2D-animation.R')

## 3D rendering / animation of WB stacks
source('rayshader-viz.R')

