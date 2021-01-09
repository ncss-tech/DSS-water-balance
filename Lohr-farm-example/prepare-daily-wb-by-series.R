library(hydromad)
library(aqp)
library(soilDB)
library(reshape2)
library(latticeExtra)
library(sharpshootR)

source('local-functions.R')

# daily climate data
load('cached-data/daily-climate-data.rda')

# component hyd data
load('cached-data/ssurgo-component-data.rda')

# # get factor levels for series
osd <- fetchOSD(s$agg$compname, extended = TRUE)
gc <- vizGeomorphicComponent(osd$geomcomp)
# gc$fig



## iterate over soils and perform water balance
wb.series <- list()
for(i in 1:nrow(s$agg)) {
  
  # current data
  d.i <- s$agg[i, ]
  series.i <- d.i[['compname']]
  
  # run water balance
  wb <- WB.simple(PPT = dm$prcp..mm.day., PET = ET$ET.Daily, D = dm$date, 
                  Sb = d.i$Sb, fc=d.i$FC, thick_mm = d.i$corrected_depth * 10,
                  S_0 = 1, a.ss = 0.03, M = 0, etmult = 1, a.ei = 0)
  
  # add series label
  wb[['series']] <- series.i
  
  wb.series[[series.i]] <- wb
}

# flatten to DF
wb.series <- do.call('rbind', wb.series)

## careful!!! this is not well tested
# set series levels
wb.series$series <- factor(wb.series$series, levels=s$agg$compname[gc$order])

## moisture state classification
# join with aggregate data, likely a better way to do this..?
wb.series <- merge(wb.series, s$agg, by.x='series', by.y='compname', all.x=TRUE, sort=FALSE)

# classify moisture state
wb.series$state <- with(wb.series, moistureState(VWC, U, sat, fc, pwp))

## looks OK
# table(wb.series$series, wb.series$state)


# months for grouping
wb.series$month <- months(wb.series$date, abbreviate = TRUE)
wb.series$month <- factor(wb.series$month, levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))

## weeks for grouping
wb.series$week <- factor(format(wb.series$date, '%U'))


save(wb.series, file='cached-data/wb-by-series.rda')
