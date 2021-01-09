
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



## example coordinates
lamoni <- SpatialPoints(cbind(-92.37841, 41.40046), proj4string = CRS('+proj=longlat +datum=WGS84'))
holland <- SpatialPoints(cbind(-120.29323, 38.01652), proj4string = CRS('+proj=longlat +datum=WGS84'))
frederick <- SpatialPoints(cbind(-85.49610, 37.19396), proj4string = CRS('+proj=longlat +datum=WGS84'))
sierra <- SpatialPoints(cbind(-120.35900, 37.98115), proj4string = CRS('+proj=longlat +datum=WGS84'))
diablo <- SpatialPoints(cbind(-121.77100, 37.368402), proj4string = CRS('+proj=longlat +datum=WGS84'))
drummer <- SpatialPoints(cbind(-88.49899, 40.00966), proj4string = CRS('+proj=longlat +datum=WGS84'))
clarno <- SpatialPoints(cbind(-97.62537, 44.25341), proj4string = CRS('+proj=longlat +datum=WGS84'))
pierre <- SpatialPoints(cbind(-102.95219, 43.35561), proj4string = CRS('+proj=longlat +datum=WGS84'))


## NC NCSS "Centralia
centralia <- SpatialPoints(cbind(-92.11446, 39.23652), proj4string = CRS('+proj=longlat +datum=WGS84'))

# https://casoilresource.lawr.ucdavis.edu/gmap/?loc=39.22880,-92.11871,z16
adco <- SpatialPoints(cbind(-92.11871, 39.22880), proj4string = CRS('+proj=longlat +datum=WGS84'))


## NC NCSS "pasture"
pasture <- SpatialPoints(cbind(-86.69957, 38.4595), proj4string = CRS('+proj=longlat +datum=WGS84'))

## ARS research farm
arf <- SpatialPoints(cbind(-93.97671, 35.09150), proj4string = CRS('+proj=longlat +datum=WGS84'))

# p <- rbind(diablo, drummer, sierra, clarno, lamoni, holland, frederick, pierre)
# 
# p <- rbind(sierra)
# 
# p <- rbind(drummer, pierre)
# 
# p <- sierra

# p <- centralia

p <- arf

## iteration over coordinates: no buffer
res <- pblapply(1:length(p), function(i) {
  suppressMessages(getDataRunModel(pp = p[i, ], modelDepth = 100, bufferRadiusMeters = 1))
})



## Centralia site

res <- pblapply(1:length(p), function(i) {
  suppressMessages(getDataRunModel(pp = p[i, ], modelDepth = 100, bufferRadiusMeters = 1500))
})



## stack into DF
d <- do.call('rbind', res)
head(d)


# convert series into a factor for simpler plotting
d$series <- factor(d$series)
levels(d$series)

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

# colors / style
ll <- levels(rs.prop$state)
n.states <- length(ll)
ms.colors <- brewer.pal(n.states, 'Spectral')

trellis.par.set(list(superpose.polygon=list(col=ms.colors, border=ms.colors)))

sK <- simpleKey(text = ll, space='top', columns=n.states, rectangles = TRUE, points=FALSE, cex=1)



## single series ~ date levelplot
## super ugly
# levelplot(state ~ date * series, data=d, col.regions=viridis, at=0:5)


## TODO: convert into a function, with adjustable state threshold
# proportion of DOY (over all years) with state < threshold
rs.doy <- lapply(rs, function(i) {
  
  # iterate over DOY (records = years)
  i.doy <- split(i, i$doy)
  
  
  state.lt.conditon.prob <- sapply(i.doy, function(j) {
    
    # there may be case where there are no TRUE cases
    # converting to a factor ensures table dimensions are consistent
    tab <- table(factor(j$state <= 'moist', levels=c('FALSE', 'TRUE')))
    
    # convert to proportions
    tab <- prop.table(tab)
    return(tab[['TRUE']])
  })
  
  res <- data.frame(series=i$series[1], doy=names(state.lt.conditon.prob), Pr=state.lt.conditon.prob, stringsAsFactors = FALSE)
  
  return(res)
})

rs.doy <- do.call('rbind', rs.doy)


stripe.colors <- colorRampPalette(rev(ms.colors), interpolate = 'spline', space = 'Lab')

levelplot(Pr ~ as.numeric(doy) * series, data=rs.doy, 
          col.regions=stripe.colors,
          xlab = 'Day of the Year',
          ylab = '',
          main='Pr(Soil at Field Capacity or Drier)\n1988-2018',
          scales=list(x=list(tick.number=25))
          )




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




### Monthly Intervals ONLY ###

barchart(proportion ~ series | interval, groups = state, 
         main='Expected Soil Moisture State\n1988-2018',
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         layout=c(4,3),
         scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)





barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\n1988-2018',
         subset=series %in% c('Clarno', 'Diablo', 'Drummer', 'Frederick', 'Sierra'),
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         # layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(draw=FALSE, relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)


s <- c('Chawanakee', 'Holland', 'Shaver')

## OSDs for misc. figures
osd <- fetchOSD(s)

SoilTaxonomyDendrogram(osd, width=0.3, cex.taxon.labels = 1, cex.names=1.125, cex.id=1)



barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\n1988-2018',
         subset=series %in% s,
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         # layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(draw=FALSE, relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)


s <- c('Lamoni', 'Drummer', 'Sierra', 'Diablo')

## OSDs for misc. figures
osd <- fetchOSD(s)

SoilTaxonomyDendrogram(osd, width=0.3, cex.taxon.labels = 1, cex.names=1.125, cex.id=1)



barchart(proportion ~ interval | series, groups = state, 
         main='Expected Weekly Soil Moisture State\n1988-2018',
         subset=series %in% s,
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         box.ratio = 3, 
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         # layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(draw=FALSE, relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)



# # a different take: "on average, when do the soils
# xyplot(proportion ~ interval | state, groups = series,
#               subset = state <= 'moist',
#               main='Expected Soil Moisture State\n1988-2018',
#               data = rs.prop, type=c('l', 'g'),
#               xlab = '', ylab='Proportion',
#               as.table=TRUE,
#               auto.key=list(lines=TRUE, points=FALSE, columns=4),
#               strip=strip.custom(bg=grey(0.9)),
#               par.strip.text=list(cex=1.25),
#               scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=90)),
#               par.settings=list(superpose.line=list(col=c('firebrick', 'orange', 'darkgreen', 'royalblue'), lwd = 2, lend = 1))
# )


