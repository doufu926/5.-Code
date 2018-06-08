# recency feature
FeatureRecency <- function(visits.all,cust.target,cutoff){
  #################################################################
  # recency
  temp <- subset(visits.all,visits.all$JOB_ORD_DT<cutoff)
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  temp.last <- temp[!duplicated(temp$VIN_NO,fromLast = TRUE),]
  temp.last$recency <- as.numeric(cutoff-temp.last$JOB_ORD_DT)
  cust.target$recency <- temp.last$recency[match(cust.target$VIN_NO,temp.last$VIN_NO)]
  
  #################################################################
  # average interval
  cust.target$interval_mean <- ifelse(cust.target$loyal==1,cust.target$visit_year/cust.target$visit,ifelse(cust.target$visit==1,NA,cust.target$visit_year/(cust.target$visit-1)))

  #################################################################
  # maximum interval
  # calculate service interval since purchase
  temp$SRV_INT <- as.numeric(temp$JOB_ORD_DT-temp$VEH_SOLD_DT)
  # filter customers service time <0 days from purchase, presales customers or revisit customers
  temp <- subset(temp, temp$SRV_INT>=0)
  # calculate service interval between two service
  temp$SRV_INT2 <- ave(temp$SRV_INT, temp$VIN_NO, FUN = function(x) c(min(x), diff(x)))
  temp$SRV_INT3 <- ave(temp$SRV_INT, temp$VIN_NO, FUN = function(x) c(0, diff(x)))
  # calculate max interval, if max interval is the first service, find the next largest (newly retained customer)
  temp1 <- aggregate(SRV_INT2~VIN_NO, data=temp, max)
  temp2 <- aggregate(SRV_INT3~VIN_NO, data=temp, max) 
  temp2$SRV_INT3[temp2$SRV_INT3==0] <- Inf
  temp2$SRV_INT2 <- temp1$SRV_INT2
  temp2$SRV_INT_MAX <- apply(temp2[,c('SRV_INT3','SRV_INT2')],1,min)
  cust.target$interval_max <- temp2$SRV_INT_MAX[match(cust.target$VIN_NO,temp2$VIN_NO)]  # assign to each record
  
  return(cust.target)
}