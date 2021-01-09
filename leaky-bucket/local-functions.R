
## get data, run model, summarize
getDataRunModel <- function(pp, modelDepth=100, bufferRadiusMeters=1) {
  
  # get elevation
  pp.e <- get_elev_point(pp)$elevation
  
  # coordinates for DAYMET
  coords <- coordinates(pp)  
  
  # DAYMET www lookup
  dm <- getDayMet(x=coords[, 1], y=coords[, 2], start_yr = 1988, end_yr = 2018)
  
  ## estimate PET from DAYMET
  ET <- estimatePET(dm, elevation = pp.e)
  
  # SSURGO data
  # transform to planar coordinate system for buffering
  p.aea <- spTransform(pp, CRS('+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs '))
  
  # buffer
  p.aea <- gBuffer(p.aea, width = bufferRadiusMeters)
  # transform back to WGS84 GCS
  p.buff <- spTransform(p.aea, CRS('+proj=longlat +datum=WGS84'))
  # convert to WKT
  p.wkt <- writeWKT(p.buff)
  
  q <- paste0("SELECT mapunit.mukey, cokey, comppct_r, compkind, compname
FROM 
mapunit
JOIN component ON mapunit.mukey = component.mukey
WHERE 
majcompflag = 'Yes'
AND mapunit.mukey IN (
SELECT * from SDA_Get_Mukey_from_intersection_with_WktWgs84('", p.wkt, "')
)")
  
  res <- SDA_query(q)
  
  # only unique instances of each component
  cokeys <- unique(res$cokey)
  
  ## TODO: there is no accounting for two components with the same name (e.g. phases)
  # get SSURGO hydrologic data for select components
  s <- getSSURGO_hydro_data(cokeys = cokeys, max.depth = modelDepth)
  
  ## TODO: save / return `s` for use outside of this function
  
  ## OR: stop here, and process in another function
  
  
  ## TODO: abstract to a function
  
  ## run model
  # iterate over soils and perform water balance
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
  
  # ## careful!!! this is not well tested
  # # set series levels
  # wb.series$series <- factor(wb.series$series, levels=s$agg$compname[gc$order])
  
  ## moisture state classification
  # join with aggregate data, likely a better way to do this..?
  wb.series <- merge(wb.series, s$agg, by.x='series', by.y='compname', all.x=TRUE, sort=FALSE)
  
  # classify moisture state
  wb.series$state <- with(wb.series, moistureState(VWC, U, sat, fc, pwp))
  
  # months for grouping
  wb.series$month <- months(wb.series$date, abbreviate = TRUE)
  wb.series$month <- factor(wb.series$month, levels=c('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'))
  
  # weeks for grouping
  wb.series$week <- factor(format(wb.series$date, '%U'))
  
  # days for grouping
  wb.series$doy <- factor(format(wb.series$date, '%j'))
  
  return(wb.series)
}











# ad hoc delineation of WVC based on
# VWC: vector of VWC
# U: vector of surplus water (mm)
# sat: satiation water content
# fc: field capacity water content
# pwp: permanent wilting point water content
moistureState <- function(VWC, U, sat, fc, pwp) {
  ms <- rep(NA, times=length(VWC))
  
  # dry = midpoint between fc and pwp
  dry.thresh <- (fc + pwp) / 2
  
  # between FC and saturation
  ms[VWC > fc] <- 'very moist'
  # between dry and FC
  ms[VWC <= fc & VWC > dry.thresh] <- 'moist'
  # dry
  ms[VWC <= dry.thresh & VWC > pwp] <- 'dry'
  # at or below PWP
  ms[VWC <= pwp] <- 'very dry'
  # surplus > 2mm -> saturation or flooded
  ms[U > 2] <- 'wet / runoff'
  # surplus > 10mm
  # ms[U > 10] <- 'saturated'
  
  # set levels
  ms <- factor(ms, levels=c('very dry', 'dry', 'moist', 'very moist', 'wet / runoff'), ordered = TRUE)
  
  return(ms)
}


# table created via: table(i$interval, i$state)
moistureStateProportions <- function(tab) {
  ss <- sweep(tab, MARGIN = 1, STATS = rowSums(tab), FUN = '/')
  ss <- as.data.frame(ss)
  names(ss) <- c('interval', 'state', 'proportion')
  return(ss)
}


# cokeys: vector of cokeys in current SSURGO
# max.depth: target depth of aggregation, corrected later by real soil depth as reported by slab()
getSSURGO_hydro_data <- function(cokeys, max.depth) {
  in.statement <- format_SQL_in_statement(cokeys)
  
  # assemble SDA query
  sql <- sprintf("
                 SELECT chorizon.cokey, compname, hzname, hzdept_r AS hz_top, hzdepb_r AS hz_bottom,
                 (hzdepb_r - hzdept_r) AS thick,
                 wsatiated_r / 100.0 AS sat,
                 wthirdbar_r / 100.0 AS fc,
                 wfifteenbar_r / 100.0 as pwp,
                 awc_r as awc,
                 -- catch strange cases where soil_fraction is NULL
                 COALESCE(soil_fraction, 1) as soil_fraction
                 FROM component 
                 JOIN chorizon ON component.cokey = chorizon.cokey
                 -- soil_fraction = 100 pct - (rock fragment volume pct)
                 LEFT JOIN (
                 SELECT chkey, (100.0 - sum(COALESCE(fragvol_r, 0))) / 100.0 as soil_fraction
                 FROM chfrags
                 GROUP BY chkey
                 ) AS frags ON chorizon.chkey = frags.chkey
                 WHERE chorizon.cokey IN %s
                 AND wsatiated_r IS NOT NULL
                 ORDER BY compname, hzdept_r;
                 ", in.statement 
)

# get via SDA
s <- SDA_query(sql)

# init SPC for slab()
depths(s) <- cokey ~ hz_top + hz_bottom
site(s) <- ~ compname

# weighted median values
agg.soil.data <- slab(s, compname ~ sat + fc + pwp + awc + soil_fraction, slab.structure = c(0, max.depth))
# retain medians for now
agg.soil.data.median <- dcast(agg.soil.data, compname ~ variable, value.var = 'p.q50')

# use real depth: contributing fraction * max.depth
real.depths <- unique(agg.soil.data[, c('compname', 'contributing_fraction')])
real.depths$corrected_depth <- real.depths$contributing_fraction * max.depth

# join corrected depths with aggregate data
agg.soil.data.median <- merge(agg.soil.data.median, real.depths, by='compname', all.x=TRUE, sort=FALSE)


## TODO return lower / upper limits

## max water storage (mm)
# Sb = thickness * satiated WC * soil_fraction
agg.soil.data.median$Sb <- with(agg.soil.data.median, corrected_depth * sat * soil_fraction) * 10

## field capacity,  fraction of total storage
agg.soil.data.median$FC <- with(agg.soil.data.median, (corrected_depth * fc) / (corrected_depth * sat))

## available water storage (mm) 
# after drainage / run-off / utilization
# includes RF adjustment
agg.soil.data.median$AWS <- with(agg.soil.data.median, (corrected_depth * awc)) * 10

# PWP equivalent depths (mm)
agg.soil.data.median$PWP <- with(agg.soil.data.median, (pwp * corrected_depth)) * 10

# pacakge and return
res <- list(SPC=s, agg=agg.soil.data.median)
return(res)
}


# x: long
# y: lat
# start_yr
# end_yr
getDayMet <- function(x, y, start_yr, end_yr) {
  
  d <- download_daymet("daymet",
                       lat = y,
                       lon = x,
                       start = start_yr,
                       end = end_yr,
                       internal = TRUE
  )
  
  # keep only the data
  d <- d$data
  
  # date for plotting and ET estimation
  d$date <- as.Date(sprintf('%s %s', d$year, d$yday), format="%Y %j")
  return(d)
}


# d: DAYMET data with Date added
estimatePET <- function(d, elevation=361) {
  
  # compile into zoo objects
  Tmax <- zoo(d$tmax..deg.c., d$date)
  Tmin <- zoo(d$tmin..deg.c., d$date)
  
  # daily total SRAD in MJ/sq.m
  # https://daymet.ornl.gov/overview
  Rs <- zoo(d$srad..W.m.2. * d$dayl..s. / 1e6, d$date)
  
  # compile into expected input format
  climate.data <- list(Date.daily=d$date, Tmax=Tmax, Tmin=Tmin, Rs=Rs)
  
  # copy constants and edit
  data(constants)
  cs <- constants
  
  # only need to modify elevation
  cs$Elev <- elevation
  
  # this works
  ET <- ET.Makkink(climate.data, constants=cs, ts="daily", solar="data", save.csv=FALSE)
  
  return(ET)
}


## leaky bucket model
# PPT: precipitation series
# PET: potential ET series
# D: Dates
# Sb: total storage in mm (satiated WC * thickness * soil_fraction)
# fc: field capacity fraction
# thick_mm: effective thickness for computing VWC
# ...: additional arguments to model
WB.simple <- function(PPT, PET, D, Sb, fc, thick_mm, ...) {
  
  
  # prep input / output data for model
  z <- data.frame(P = PPT, E = PET)
  
  # init model
  m <- hydromad(z, sma = "bucket", routing = NULL)
  m <- update(m, Sb = Sb, fc = fc, ...)
  
  # predictions
  res <- predict(m, return_state = TRUE)
  # combine date, inputs / predictions
  res <- data.frame(date=D, z, res)
  
  # rough approximation of VWC
  res$VWC <- res$S / thick_mm
  
  return(res)
  
}


## this is not what I thought that it was: three components in a catchment no soil horizons
WB.awbm <- function(PPT, PET, D, cap, thick, S_0=cap) {
 
  # prep input / output data for model
  z <- data.frame(P = PPT, E = PET)
  
  # compute area from thickness
  area <- vector(mode = 'numeric', length = 3)
  area[1] <- thick[1] / sum(thick)
  area[2] <- thick[2] / sum(thick)
  area[3] <- 1 - (area[1] + area[2])
  
  m <- hydromad(z, sma = "awbm", routing = "expuh")
  m <- update(m, cap.ave=mean(cap), 
              cap1=cap[1], cap2=cap[2], cap3=cap[3], 
              area1=area[1], area2=area[2], area3=area[3], 
              S1_0=S_0[1], S2_0=S_0[2], S3_0=S_0[3], 
              etmult=1
  )
  
  # predictions
  res <- predict(m, return_state = TRUE)
  # combine inputs / predictions / date
  res <- data.frame(date=D, z, res)
  
  res$S <- with(res, S1 + S2 + S3)
  
  return(res)
}


