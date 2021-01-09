library(aqp)
library(soilDB)


## not enough data
lab <- lapply(series.names, fetchKSSL, returnMorphologicData=TRUE, simplifyColors=TRUE)
lab <- union(lapply(lab, '[[', 'SPC'))

lab$taxonname <- factor(toupper(lab$taxonname))
groupedProfilePlot(lab, groups='taxonname', color='wrd_l2')


## NASIS
pedons <- fetchNASIS(from='pedons')
table(pedons$taxonname)

plot(pedons[which(pedons$taxonname == 'Frederick')[1:30],])
