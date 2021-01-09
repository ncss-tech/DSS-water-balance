library(aqp)
library(soilDB)
library(reshape2)

source('local-functions.R')

## modeling domain
model.depth.cm <- 100

## get / aggregate SSURGO data
# FY2020 cokeys, not stable over SSURGO releases..
# these correspond to the components we are working with
cokeys <- c('17469321', '17469317', '17469215', '17469245', '17469354')
s <- getSSURGO_hydro_data(cokeys = cokeys, max.depth = model.depth.cm)

plot(s$SPC, color='awc', label='compname')
s$agg

save(s, file = 'cached-data/ssurgo-component-data.rda')
