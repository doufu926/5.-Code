# frequency feature
FeatureFrequency <- function(visits.all,cust.target,cutoff){
  #################################################################
  ## total visits till cut off date
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<=cutoff)
  temp1 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp, FUN = function(x) length(x))
  cust.target$visit <- temp1$JOB_ORD_DT[match(cust.target$VIN_NO,temp1$VIN_NO)]
  
  #################################################################
  ## retained period
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  temp.first <- temp[!duplicated(temp$VIN_NO,fromLast = FALSE),]
  temp.first$visit.length <- as.numeric(cutoff-temp.first$JOB_ORD_DT) # time since first service
  temp.first$visit.length2 <- as.numeric(temp.first$JOB_ORD_DT-temp.first$VEH_SOLD_DT) # gap between first service and purchase
  temp.first$visit.length3 <- as.numeric(cutoff-temp.first$VEH_SOLD_DT) # time since purchase
  # if first service is more than 15 month after purchase, consider as newly retained, count retain period from first service, else from purchase
  temp.first$visit.length4 <- ifelse(temp.first$visit.length2>15*30,temp.first$visit.length,temp.first$visit.length3)
  cust.target$visit_year <- temp.first$visit.length4[match(cust.target$VIN_NO,temp.first$VIN_NO)]
  
  #################################################################
  ## visit/retained period
  cust.target$visit_avg <- ifelse(cust.target$visit_year<365,cust.target$visit,cust.target$visit/cust.target$visit_year*365)

  #################################################################  
  # vist in last year
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<=cutoff&visits.all$JOB_ORD_DT>cutoff-365)
  temp1 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp, FUN = function(x) length(x))
  cust.target$visit_last <- temp1$JOB_ORD_DT[match(cust.target$VIN_NO,temp1$VIN_NO)]
  cust.target$visit_last[is.na(cust.target$visit_last)] <- 0
  
  #################################################################  
  # visit trend
  cust.target$visit_trend <- cust.target$visit_last/cust.target$visit_avg
  
  return(cust.target)
}