# frequency feature ## change retain period definition, from cutoff to last service
FeatureFrequency <- function(visits.all,cust.target,cutoff){
  #################################################################
  ## total visits till cut off date
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<cutoff)
  temp1 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp, FUN = function(x) length(x))
  cust.target$visit <- temp1$JOB_ORD_DT[match(cust.target$VIN_NO,temp1$VIN_NO)]
  
  #################################################################
  ## retained period
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  # find first service for each customer
  temp.first <- temp[!duplicated(temp$VIN_NO,fromLast = FALSE),]
  # find last service for each customer
  temp.last <- temp[!duplicated(temp$VIN_NO,fromLast = TRUE),]
  # combine 
  temp.first$last <- temp.last$JOB_ORD_DT[match(temp.first$VIN_NO,temp.last$VIN_NO)]
  # calculate time intervals
  temp.first$visit.length <- as.numeric(temp.first$last-temp.first$JOB_ORD_DT) # time from first to last service
  temp.first$visit.length2 <- as.numeric(temp.first$JOB_ORD_DT-temp.first$VEH_SOLD_DT) # gap between first service and purchase
  temp.first$visit.length3 <- as.numeric(temp.first$last-temp.first$VEH_SOLD_DT) # time from purchase to last service
  # if first service is more than 15 month after purchase, consider as newly retained, count retain period from first service, else from purchase
  # if newly retained and only serviced once, retention year is NA
  temp.first$visit.length4 <- ifelse(temp.first$visit.length2>15*30,ifelse(temp.first$visit.length==0,NA,temp.first$visit.length),temp.first$visit.length3)
  cust.target$visit_year <- temp.first$visit.length4[match(cust.target$VIN_NO,temp.first$VIN_NO)]
  #################################################################
  ## retained since purchase
  # get first service interval
  cust.target$loyal <- temp.first$visit.length2[match(cust.target$VIN_NO,temp.first$VIN_NO)]
  # if first service is more than 15 month after purchase, consider as newly retained, count retain period from first service, else from purchase
  cust.target$loyal <- ifelse(cust.target$loyal>15*30,0,1)
  #################################################################
  ## visit/retained period
  # if if newly retained and only serviced once, average visit is NA
  cust.target$visit_avg <- ifelse(cust.target$loyal==1,cust.target$visit/cust.target$visit_year*365,ifelse(cust.target$visit==1,NA,(cust.target$visit-1)/cust.target$visit_year*365))
  #################################################################  
  # vist in last year
  temp.date <- as.POSIXlt(cutoff)
  temp.date$year <- temp.date$year-1
  cutoff.last <- as.Date(temp.date)
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<cutoff&visits.all$JOB_ORD_DT>=cutoff.last)
  temp1 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp, FUN = function(x) length(x))
  cust.target$visit_last <- temp1$JOB_ORD_DT[match(cust.target$VIN_NO,temp1$VIN_NO)]
  cust.target$visit_last[is.na(cust.target$visit_last)] <- 0
  
  #################################################################  
  # visit trend
  cust.target$visit_trend <- cust.target$visit_last/cust.target$visit_avg
  
  return(cust.target)
}