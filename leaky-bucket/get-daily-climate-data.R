library(daymetr)
library(Evapotranspiration)

source('local-functions.R')

# Centrailia

## daily climate data
dm <- getDayMet(x=-92.11446, y=39.23652, start_yr = 1988, end_yr = 2018)

## estimate PET from DAYMET
ET <- estimatePET(dm, elevation = 361)

save(dm, ET, file='cached-data/daily-climate-data.rda')
