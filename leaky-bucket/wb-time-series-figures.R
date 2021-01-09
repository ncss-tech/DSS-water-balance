library(latticeExtra)
library(aqp)
library(scico)

source('local-functions.R')

## daily water balance by series
load('cached-data/wb-by-series.rda')

## component data
load('cached-data/ssurgo-component-data.rda')

s.name <- 'Frederick'
z <- wb.series[which(wb.series$series == s.name &  format(wb.series$date, '%Y') %in% c('2013', '2014')), ]
model.params <- s$agg[which(s$agg$compname == s.name), ]


layout(mat = rbind(1,2), widths = c(1, 1), heights = c(1, 2))

par(mar=c(1, 4, 2, 1))
plot(z$date, z$P, type='h', col='royalblue', ylab='PPT (mm)', xlab='', las=1, cex.axis=0.75, main=unique(z$series))

cols <- NA
cols[which(z$U > 2)] <- 'red'

par(mar=c(3, 4, 2, 1))
plot(z$date, z$VWC, type='l', las=1, col='#377EB8', ylab='Volumetric Water Content (cm^3/cm^3)', xlab='')
abline(h=max(z$VWC), lty=3)
abline(h=model.params$fc, lty=3)

abline(h=model.params$pwp, lty=3)
points(x=z$date, y=rep(max(z$VWC), times=nrow(z)), pch=15, cex=0.25, col=cols)



### old

# ## check:
# layout(mat = rbind(1,2), widths = c(1, 1), heights = c(1, 2))
# 
# par(mar=c(1, 4, 2, 1))
# plot(z$date, z$P, type='h', col='royalblue', ylab='PPT (mm)', xlab='', las=1, cex.axis=0.75, main=s.name)
# 
# par(mar=c(3, 4, 2, 1))
# plot(z$date, z$S, type='l', las=1, col='firebrick', ylim=c(min(z$S), max(z$S) + max(z$U)), xlab='', cex.axis=0.75, ylab='Equivalent Water Depth (mm)')
# segments(x0 = z$date, x1= z$date, y0=model.params$Sb, y1=z$U + model.params$Sb, col='#4DAF4A')
# abline(h=model.params$Sb, lty=3)
# abline(h=model.params$FC * max(z$S), lty=3)
# abline(h=model.params$PWP, lty=3)
# 





################################## not using this ##########################

# 
# wb2 <- WB.awbm(PPT = dm$prcp..mm.day., PET = ET$ET.Daily, D = dm$date, 
#                 cap=1, thick=1, S_0=1
# )
# 
# ## check:
# plot(wb.seriesdate, res$S/sum(sda.data$thick[1:3] * 10), type='n', las=1, col='grey', lwd=2, ylim=c(0,1))
# lines(res$date, res$S1 / (sda.data$thick[1] * 10), col=1)
# lines(res$date, res$S2 / (sda.data$thick[2] * 10), col=2)
# lines(res$date, res$S3 / (sda.data$thick[3] * 10), col=3)
# abline(h=sda.data$sat[1:3], col=1:3, lty=3)
# abline(h=sda.data$fc[1:3], col=1:3, lty=2)
# abline(h=sda.data$pwp[1:3], col=1:3, lty=4)
# 
# segments(x0 = res$date, x1= res$date, y0=Sb.model, y1=res$U + Sb.model, col='#4DAF4A')
# 
# lines(dd$date, dd$prcp..mm.day., type='h')

## double-check that monthly sums of daily PET are reasonable


## more ideas: https://wiki.cyverse.org/wiki/display/~tyson_swetnam/Raster+Calculations



# ##
# # https://cran.r-project.org/web/packages/Evapotranspiration/Evapotranspiration.pdf
# 
# ## simplest
# results <-  ET.Hamon(d, constants = NULL, ts="daily", message="yes")
# 
# # looks reasonable
# plot(results$ET.Daily, type='l')
# 

