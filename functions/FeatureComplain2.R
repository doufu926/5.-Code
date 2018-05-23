# complaint feature
FeatureComplain2 <- function(visits.all,complaint.all,cust.target,cutoff){
  #################################################################
  # get complaint opened before cutoff date
  complaint.known <- subset(complaint.all,complaint.all$Open.Date<=cutoff)
  visits.all <- subset(visits.all,visits.all$JOB_ORD_DT<=cutoff)
  
  #################################################################
  # get the open date for last complain for each customer
  temp <- complaint.known[,c("Incident_ID","VIN._NO.","Open.Date")]
  temp <- na.omit(temp)
  temp <- temp[order(temp$VIN._NO.,temp$Open.Date),]
  # get last record
  temp.last <- temp[!duplicated(temp$VIN._NO.,fromLast = TRUE),]
  
  #################################################################
  # assign last complaint date to visits
  visits.all$complain_last=temp.last$Open.Date[match(visits.all$VIN_NO,temp.last$VIN._NO.)]
  visits.all <- na.omit(visits.all)
  # find visits after complain
  visits.all$keep <- (visits.all$JOB_ORD_DT>visits.all$complain_last)
  temp <- aggregate(keep~VIN_NO, data = visits.all, FUN = sum)
  # assign to cust
  cust.target$complain_visit <- temp$keep[match(cust.target$VIN_NO,temp$VIN_NO)]
  
  return(cust.target)
}