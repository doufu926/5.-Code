# parts feature
Featureparts <- function(data.all,cust.target,cutoff){
  #################################################################
  ## parts per visit
  # filter customer record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff)
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","PART_NO","Part.Issue.Qty")]
  temp$Part.Issue.Qty[is.na(temp$Part.Issue.Qty)]=0
  # aggregate by visit
  temp2 <- aggregate(Part.Issue.Qty~VIN_NO+JOB_ORD_NO, data = temp, FUN = sum)
  # parts/visit on average
  temp3 <- aggregate(Part.Issue.Qty~VIN_NO, data = temp2, FUN = mean)
  cust.target$part_avg <- temp3$Part.Issue.Qty[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  #################################################################
  ## parts per visit in last year
  # filter customer record before cutoff
  temp.date <- as.POSIXlt(cutoff)
  temp.date$year <- temp.date$year-1
  cutoff.last <- as.Date(temp.date)
  temp <- subset(data.all,data.all$JOB_ORD_DT<cutoff&data.all$JOB_ORD_DT>=cutoff.last)
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","PART_NO","Part.Issue.Qty")]
  temp$Part.Issue.Qty[is.na(temp$Part.Issue.Qty)]=0
  # aggregate by visit
  temp2 <- aggregate(Part.Issue.Qty~VIN_NO+JOB_ORD_NO, data = temp, FUN = sum)
  # parts/visit on average
  temp3 <- aggregate(Part.Issue.Qty~VIN_NO, data = temp2, FUN = mean)
  cust.target$part_last <- temp3$Part.Issue.Qty[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  return(cust.target)
}