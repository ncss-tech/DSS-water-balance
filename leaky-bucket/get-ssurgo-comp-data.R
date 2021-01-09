library(aqp)
library(soilDB)
library(reshape2)
library(RColorBrewer)

source('local-functions.R')

## modeling domain
model.depth.cm <- 100

## get / aggregate SSURGO data
# FY2020 cokeys, not stable over SSURGO releases..
# these correspond to the components we are working with
cokeys <- c('17623572', '17623570', '17623377', '17623394')
s <- getSSURGO_hydro_data(cokeys = cokeys, max.depth = model.depth.cm)

par(mar=c(0,0,3,1))
cols <- brewer.pal(9, 'Spectral')
plotSPC(s$SPC, color='awc', label='compname', col.palette = cols)


# plot(s$SPC, color='fc', label='compname')

knitr::kable(s$agg, digits = 3)

save(s, file = 'cached-data/ssurgo-component-data.rda')
