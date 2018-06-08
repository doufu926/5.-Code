# extract service timing features
FeatureDuration <- function(data.all,cust.target,cutoff){
  #############################################################
  ## average duration of service
  # get all service record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff)
  # select the useful columns
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","ACT_RPR_START_DT","ACT_RPR_END_DT","RPR_START_TM","RPR_END_TM")]
  # fill date with service date if no data
  temp$ACT_RPR_START_DT <- ifelse(is.na(temp$ACT_RPR_START_DT),temp$JOB_ORD_DT,temp$ACT_RPR_START_DT)
  # get unique service visits
  temp <- na.omit(temp)
  temp <- unique(temp)
  # calculate service duration in hours
  temp$duration <- as.numeric(temp$ACT_RPR_END_DT-temp$ACT_RPR_START_DT)*24+temp$RPR_END_TM-temp$RPR_START_TM
  # calculate average duration per visit
  temp2 <- aggregate(duration~VIN_NO, data = temp, FUN = mean)
  cust.target$duration_avg <- temp2$duration[match(cust.target$VIN_NO,temp2$VIN_NO)]

  #############################################################
  ## average duration of service in last year
  # get all service record in last year
  temp.date <- as.POSIXlt(cutoff)
  temp.date$year <- temp.date$year-1
  cutoff.last <- as.Date(temp.date)
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff&data.all$JOB_ORD_DT>=cutoff.last) 
  # select the useful columns
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","ACT_RPR_START_DT","ACT_RPR_END_DT","RPR_START_TM","RPR_END_TM")]
  # fill date with service date if no data
  temp$ACT_RPR_START_DT <- ifelse(is.na(temp$ACT_RPR_START_DT),temp$JOB_ORD_DT,temp$ACT_RPR_START_DT)
  # get unique service visits
  temp <- na.omit(temp)
  temp <- unique(temp)
  # calculate service duration in hours
  temp$duration <- as.numeric(temp$ACT_RPR_END_DT-temp$ACT_RPR_START_DT)*24+temp$RPR_END_TM-temp$RPR_START_TM
  # calculate average duration per visit
  temp2 <- aggregate(duration~VIN_NO, data = temp, FUN = mean)
  cust.target$duration_last <- temp2$duration[match(cust.target$VIN_NO,temp2$VIN_NO)]  
  
  return(cust.target)
}