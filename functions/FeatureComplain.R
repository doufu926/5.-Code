# complaint feature
FeatureComplain <- function(complaint.all,cust.target,cutoff){
  #################################################################
  # get complaint opened before cutoff date
  complaint.known <- subset(complaint.all,complaint.all$Open.Date<=cutoff)
  
  #################################################################
  # total number
  temp <- aggregate(Incident_ID~VIN._NO., data = complaint.known, FUN = function(x) {NROW(x)})
  cust.target$complain_all <- temp$Incident_ID[match(cust.target$VIN_NO,temp$VIN._NO.)]
  cust.target$complain_all[is.na(cust.target$complain_all)]=0

  #################################################################
  # severity number
  temp <- subset(complaint.known,complaint.known$Serverity.Type==2)
  temp2 <- aggregate(Incident_ID~VIN._NO., data = temp, FUN = function(x) {NROW(x)})
  cust.target$complain_severity <- temp2$Incident_ID[match(cust.target$VIN_NO,temp2$VIN._NO.)]
  cust.target$complain_severity[is.na(cust.target$complain_severity)]=0
  
  #################################################################
  # unsatisfied number
  temp <- subset(complaint.known, complaint.known$Satifaction.result==0)
  temp2 <- aggregate(Incident_ID~VIN._NO., data = temp, FUN = function(x) {NROW(x)})
  cust.target$complain_unsatisfied <- temp2$Incident_ID[match(cust.target$VIN_NO,temp2$VIN._NO.)]
  cust.target$complain_unsatisfied[is.na(cust.target$complain_unsatisfied)]=0
  
  #################################################################
  # average time taken for resolve
  temp <- complaint.known[,c("Incident_ID","VIN._NO.","Open.Date","Summary.DT")]
  temp <- na.omit(temp)
  temp$time <- as.numeric(temp$Summary.DT-temp$Open.Date)
  temp2 <- aggregate(time~VIN._NO., data = temp, FUN = mean)
  cust.target$complain_time <- temp2$time[match(cust.target$VIN_NO,temp2$VIN._NO.)]
  
  #################################################################
  # open complaint
  temp <- complaint.known[,c("Incident_ID","VIN._NO.","Open.Date","Summary.DT")]
  temp <- subset(temp,is.na(temp$Summary.DT))
  temp2 <- aggregate(Incident_ID~VIN._NO., data = temp, FUN = function(x) {NROW(x)})
  cust.target$complain_open <- temp2$Incident_ID[match(cust.target$VIN_NO,temp2$VIN._NO.)]
  cust.target$complain_open[is.na(cust.target$complain_open)]=0
  
  return(cust.target)
}