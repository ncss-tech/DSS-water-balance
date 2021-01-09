library(latticeExtra)
library(reshape2)
library(knitr)
library(RColorBrewer)

# library(scico)

source('local-functions.R')

## daily water balance by series
load('cached-data/wb-by-series.rda')


### temporary re-leveling of components
wb.series$series <- factor(wb.series$series, levels = c('Gillender', 'Amador', 'Sierra', 'Nedsgulch'))

## classify using moisture state threshold
wb.series$state_thresh <- factor(wb.series$state > 'moist')


## quick check
bwplot(series ~ VWC | month, data=wb.series, as.table=TRUE)
bwplot(month ~ VWC | series, data=wb.series, as.table=TRUE)

table(wb.series$series, wb.series$state)

kable(prop.table(table(wb.series$series, wb.series$state), margin = 1), digits = 2)

# not that interesting over entire period of record
kable(prop.table(table(wb.series$series, wb.series$state_thresh), margin = 1), digits = 2)


## monthly moisture state proportions by series
rs <- split(wb.series, wb.series$series)

## TODO: generalize and wrap with function
## adjust binning interval: doy, week, month
rs.prop <- lapply(rs, function(i) {
  tab <- table(i$week, i$state)
  ss <- moistureStateProportions(tab)
  ss$series <- unique(i$series)
  return(ss)
})

rs.prop <- do.call('rbind', rs.prop)


## TODO: better colors, proportion ponded is very small
# hmm, just ok
ms.colors <- brewer.pal(6, 'Spectral')
# eh
# ms.colors <- rev(viridis::viridis(6))
# ok, but hard to look at
# ms.colors <- scico(6, palette = 'roma')



# viz
p.1 <- barchart(series ~ proportion | interval, groups = state, 
         main='Expected Soil Moisture State\n1988-2018',
         data = rs.prop, horiz = TRUE, stack = TRUE, xlab = "Proportion",
         as.table=TRUE, auto.key=list(columns=6),
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         scales=list(x=list(alternating=1), cex=1),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1))
)


trellis.par.set(list(superpose.polygon=list(col=ms.colors)))
sK <- simpleKey(text = c('very dry', 'dry', 'moist', 'very moist', 'wet', 'saturated'), space='top', columns=6, rectangles = TRUE, points=FALSE, cex=1)

p.2 <- barchart(proportion ~ interval | series, groups = state, 
         main='Expected Soil Moisture State\n1988-2018',
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         layout=c(2,2),
         scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.75, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           # panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)


## just 1
p.3 <- barchart(proportion ~ interval | series, groups = state, 
                subset= series == 'Sierra',
                main='Expected Soil Moisture State\n1988-2018',
                data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
                as.table=TRUE,
                key=sK,
                strip=strip.custom(bg=grey(0.9)),
                par.strip.text=list(cex=1.25),
                scales=list(y=list(alternating=3, cex=1.25), x=list(relation='free', alternating=1, cex=1, rot=90)),
                par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
                # this breaks auto.key, thus requires simpleKey()
                panel=function(...) {
                  panel.abline(h=seq(0, 1, by=0.1), v=1:length(levels(rs.prop$interval)), col=grey(0.9))
                  panel.barchart(...)
                }
)


# a different take: "on average, when do the soils 
p.4 <- xyplot(proportion ~ interval | state, groups = series,
                subset = state == 'very dry',
                main='Expected Soil Moisture State\n1988-2018',
                data = rs.prop, type=c('l', 'g'),
                xlab = '', ylab='Proportion',
                as.table=TRUE,
                auto.key=list(lines=TRUE, points=FALSE, columns=4),
                strip=strip.custom(bg=grey(0.9)),
                par.strip.text=list(cex=1.25),
                scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=90)),
                par.settings=list(superpose.line=list(col=c('firebrick', 'orange', 'darkgreen', 'royalblue'), lwd = 2, lend = 1))
)


## save
png(file='figures/monthly-ms-proportions-months.png', width=1200, height=700, res=96, antialias = 'cleartype')
print(p.1)
dev.off()

png(file='figures/monthly-ms-proportions-series.png', width=1200, height=700, res=96, antialias = 'cleartype')
print(p.2)
qdev.off()

png(file='figures/monthly-ms-proportions-Frederick.png', width=900, height=550, res=96, antialias = 'cleartype')
print(p.3)
dev.off()


## fancy formatting for slide
fp <- dcast(rs.prop[rs.prop$series == 'Frederick', ], month ~ state, value.var = 'proportion')

kable(fp, row.names = FALSE, digits = 2, format = 'pandoc')

### needs work...

## weekly data by series
## cells using levelplot

# ## monthly moisture state proportions by series
# rs <- split(wb.series, wb.series$series)
# 
# rs.week <- lapply(rs, function(i) {
#   tab <- table(i$week, i$state)
#   
#   wk <- dimnames(tab)[[1]]
#   nm <- dimnames(tab)[[2]]
#   
#   most.freq.state <- nm[apply(tab, 1, which.max)]
#   
#   res <- data.frame(week=factor(wk), state=factor(most.freq.state, levels=levels(i$state)), series=unique(i$series))
#   return(res)
# })
# 
# rs.week <- do.call('rbind', rs.week)
# rs.week$series <- factor(rs.week$series, levels=levels(wb.series$series))
# 
# levelplot(state ~ week * series, data=rs.week, col.regions = ms.colors, at=0:6)



