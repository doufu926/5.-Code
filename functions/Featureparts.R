# parts feature
Featureparts <- function(data.all,cust.target,cutoff){
  #################################################################
  ## parts on average
  # filter customer record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff)
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","PART_NO","Part.Issue.Qty")]
  temp$Part.Issue.Qty[is.na(temp$Part.Issue.Qty)]=0
  # aggregate by visit
  temp2 <- aggregate(Part.Issue.Qty~VIN_NO+JOB_ORD_NO, data = temp, FUN = sum)
  # parts/visit on average
  temp3 <- aggregate(Part.Issue.Qty~VIN_NO, data = temp2, FUN = mean)
  cust.target$part_avg <- temp3$Part.Issue.Qty[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  #################################################################
  ## parts on average
  # filter customer record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff&data.all$JOB_ORD_DT>cutoff-365)
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","PART_NO","Part.Issue.Qty")]
  temp$Part.Issue.Qty[is.na(temp$Part.Issue.Qty)]=0
  # aggregate by visit
  temp2 <- aggregate(Part.Issue.Qty~VIN_NO+JOB_ORD_NO, data = temp, FUN = sum)
  # parts/visit on average
  temp3 <- aggregate(Part.Issue.Qty~VIN_NO, data = temp2, FUN = mean)
  cust.target$part_last <- temp3$Part.Issue.Qty[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  return(cust.target)
}