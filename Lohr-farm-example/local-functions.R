
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
  ms[U > 2] <- 'wet'
  # surplus > 10mm
  ms[U > 10] <- 'saturated'
  
  # set levels
  ms <- factor(ms, levels=c('very dry', 'dry', 'moist', 'very moist', 'wet', 'saturated'))
  
  return(ms)
}


# table created via: table(i$month, i$state)
moistureStateProportions <- function(tab) {
  ss <- sweep(tab, MARGIN = 1, STATS = rowSums(tab), FUN = '/')
  ss <- as.data.frame(ss)
  names(ss) <- c('month', 'state', 'proportion')
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
                 soil_fraction
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


## this is not what I thought that it was: thee components in a catchment no soil horizons
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


