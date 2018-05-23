# recency feature
FeatureRecency <- function(visits.all,cust.target,cutoff){
  #################################################################
  # recency
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<=cutoff)
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  temp.last <- temp[!duplicated(temp$VIN_NO,fromLast = TRUE),]
  temp.last$recency <- as.numeric(cutoff-temp.last$JOB_ORD_DT)
  cust.target$recency <- temp.last$recency[match(cust.target$VIN_NO,temp.last$VIN_NO)]
  
  #################################################################
  # average interval
  cust.target$interval_mean <- cust.target$visit_year/cust.target$visit

  return(cust.target)
}