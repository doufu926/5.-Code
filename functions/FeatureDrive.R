# driving feature
FeatureDrive <- function(data.all,cust.target,cutoff){
  #################################################################
  # average milage/day based on last service before cut off date
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff)
  temp <- temp[,c("VIN_NO","JOB_ORD_DT","MILEAGE_OUT","VEH_SOLD_DT")]
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  temp <- temp[!duplicated(temp$VIN_NO,fromLast = TRUE),]
  temp <- na.omit(temp)
  temp$mielage_day <- temp$MILEAGE_OUT/as.numeric(temp$JOB_ORD_DT-temp$VEH_SOLD_DT)
  cust.target$mileage_day <- temp$mielage_day[match(cust.target$VIN_NO,temp$VIN_NO)]
  #################################################################
  # estimated total mileage till cut off date
  cust.target$milage <- cust.target$ownership*cust.target$mileage_day
  #################################################################
  # free labor till cutoff
  cust.target$warranty_labor <- 0
  cust.target$warranty_labor[cust.target$milage<=50000 & cust.target$ownership<=2.5*365]=1
  #################################################################
  # last PM
  # find all PM operations
  library(Hmisc)
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff)
  temp <- subset(temp,temp$PM==1)
  # find last service
  temp <- temp[,c("VIN_NO","JOB_ORD_DT","OP_CD")]
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  temp <- temp[!duplicated(temp$VIN_NO,fromLast = TRUE),]
  cust.target$last_PM <- as.numeric(temp$OP_CD[match(cust.target$VIN_NO,temp$VIN_NO)])/1000
  
  return(cust.target)
}