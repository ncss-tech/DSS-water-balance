






















# 
# 
# ## this is not what I thought that it was: three components in a catchment no soil horizons
# WB.awbm <- function(PPT, PET, D, cap, thick, S_0=cap) {
#  
#   # prep input / output data for model
#   z <- data.frame(P = PPT, E = PET)
#   
#   # compute area from thickness
#   area <- vector(mode = 'numeric', length = 3)
#   area[1] <- thick[1] / sum(thick)
#   area[2] <- thick[2] / sum(thick)
#   area[3] <- 1 - (area[1] + area[2])
#   
#   m <- hydromad(z, sma = "awbm", routing = "expuh")
#   m <- update(m, cap.ave=mean(cap), 
#               cap1=cap[1], cap2=cap[2], cap3=cap[3], 
#               area1=area[1], area2=area[2], area3=area[3], 
#               S1_0=S_0[1], S2_0=S_0[2], S3_0=S_0[3], 
#               etmult=1
#   )
#   
#   # predictions
#   res <- predict(m, return_state = TRUE)
#   # combine inputs / predictions / date
#   res <- data.frame(date=D, z, res)
#   
#   res$S <- with(res, S1 + S2 + S3)
#   
#   return(res)
# }


