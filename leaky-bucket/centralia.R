
# 2020-07-06
# be sure to use the development version
library(daymetr)
library(Evapotranspiration)

library(aqp)
library(soilDB)
library(reshape2)

library(sp)
library(rgeos)

library(elevatr)
library(hydromad)

library(sharpshootR)
library(zoo)

library(pbapply)

library(latticeExtra)
library(knitr)
library(RColorBrewer)
library(viridis)

library(ragg)

source('local-functions.R')


##
## 2020-07-06: DAYMET server not reachable (not related to VPN)
## https://github.com/bluegreen-labs/daymetr/issues/43
##



## NC NCSS "Centralia
centralia <- SpatialPoints(cbind(-92.11446, 39.23652), proj4string = CRS('+proj=longlat +datum=WGS84'))

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.22880,-92.11871,z16
adco <- SpatialPoints(cbind(-92.11871, 39.22880), proj4string = CRS('+proj=longlat +datum=WGS84'))

## NC NCSS "pasture"
pasture <- SpatialPoints(cbind(-86.69957, 38.4595), proj4string = CRS('+proj=longlat +datum=WGS84'))


p <- adco

## iteration over coordinates: no buffer
res <- pblapply(1:length(p), function(i) {
  suppressMessages(getDataRunModel(pp = p[i, ], modelDepth = 100, bufferRadiusMeters = 1))
})



## Centralia site
p <- centralia
res <- pblapply(1:length(p), function(i) {
  suppressMessages(getDataRunModel(pp = p[i, ], modelDepth = 100, bufferRadiusMeters = 1500))
})



## stack into DF
d <- do.call('rbind', res)
head(d)


# convert series into a factor for simpler plotting
d <- subset(d, subset = series %in% c('Adco', 'Armstrong', 'Belknap', 'Leonard', 'Mexico', 'Putnam'))
d$series <- factor(d$series)


## TODO consider adjusting based on hydro / hillslope pos
## automating this would require saving intermediate pieces, or 

# osd <- fetchOSD(s$agg$compname, extended = TRUE)
# gc <- vizGeomorphicComponent(osd$geomcomp)
# # gc$fig




## quick check on 30-yr proportions
kable(prop.table(table(d$series, d$state), margin = 1), digits = 2)

kable(prop.table(table(d$series, d$state < 'dry'), margin = 1), digits = 2)
kable(prop.table(table(d$series, d$state > 'very moist'), margin = 1), digits = 2)



## monthly moisture state proportions by series
rs <- split(d, d$series)

## TODO: generalize and wrap with function
## adjust binning interval: doy, week, month
rs.prop <- lapply(rs, function(i) {
  tab <- table(i$week, i$state)
  ss <- moistureStateProportions(tab)
  ss$series <- unique(i$series)
  return(ss)
})

rs.prop <- do.call('rbind', rs.prop)

## BUG: ordered moisture state factor down-graded to regular factor
##      levels are correct
rs.prop$state <- ordered(rs.prop$state)

## TODO: generalize and wrap with function
## adjust binning interval: doy, week, month
rs.monthly <- lapply(rs, function(i) {
  tab <- table(i$month, i$state)
  ss <- moistureStateProportions(tab)
  ss$series <- unique(i$series)
  return(ss)
})

rs.monthly <- do.call('rbind', rs.monthly)

## BUG: ordered moisture state factor down-graded to regular factor
##      levels are correct
rs.monthly$state <- ordered(rs.monthly$state)




# colors / style
ll <- levels(rs.prop$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)



# tinker with box.ratio

barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\nDAYMET 1988-2018',
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         box.ratio = 30,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         # layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(draw = FALSE)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)

