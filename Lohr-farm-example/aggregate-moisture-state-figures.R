library(latticeExtra)
library(reshape2)
library(knitr)
# library(scico)

source('local-functions.R')

## daily water balance by series
load('cached-data/wb-by-series.rda')

## quick check
bwplot(series ~ VWC | month, data=wb.series, as.table=TRUE)
table(wb.series$series, wb.series$state)

## monthly moisture state proportions by series
rs <- split(wb.series, wb.series$series)

rs.prop <- lapply(rs, function(i) {
  tab <- table(i$month, i$state)
  ss <- moistureStateProportions(tab)
  ss$series <- unique(i$series)
  return(ss)
})

rs.prop <- do.call('rbind', rs.prop)


## TODO: better colors, proportion ponded is very small
# hmm, just ok
ms.colors <- brewer.pal(6, 'Spectral')
# better
# ms.colors <- rev(scico(6, palette='vik'))

# viz
p.1 <- barchart(series ~ proportion | month, groups = state, 
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

p.2 <- barchart(proportion ~ month | series, groups = state, 
         main='Expected Soil Moisture State\n1988-2018',
         data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
         as.table=TRUE,
         key=sK,
         strip=strip.custom(bg=grey(0.9)),
         par.strip.text=list(cex=1.25),
         layout=c(2,3),
         scales=list(y=list(alternating=3, cex=1), x=list(relation='free', alternating=1, cex=0.85, rot=90)),
         par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
         # this breaks auto.key, thus requires simpleKey()
         panel=function(...) {
           panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
           panel.barchart(...)
         }
)


## just Frederick
p.3 <- barchart(proportion ~ month | series, groups = state, 
                subset= series == 'Frederick',
                main='Expected Soil Moisture State\n1988-2018',
                data = rs.prop, horiz = FALSE, stack = TRUE, xlab = '', ylab='Proportion',
                as.table=TRUE,
                key=sK,
                strip=strip.custom(bg=grey(0.9)),
                par.strip.text=list(cex=1.25),
                scales=list(y=list(alternating=3, cex=1.25), x=list(relation='free', alternating=1, cex=1.25)),
                par.settings=list(superpose.polygon=list(col=ms.colors, lwd = 1, lend = 1)),
                # this breaks auto.key, thus requires simpleKey()
                panel=function(...) {
                  panel.abline(h=seq(0, 1, by=0.1), v=1:12, col=grey(0.9))
                  panel.barchart(...)
                }
)

## save
png(file='figures/monthly-ms-proportions-months.png', width=1200, height=700, res=96, antialias = 'cleartype')
print(p.1)
dev.off()

png(file='figures/monthly-ms-proportions-series.png', width=1200, height=700, res=96, antialias = 'cleartype')
print(p.2)
dev.off()

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



