# delay feature
FeatureDelay <- function(data.all,cust.target,cutoff){
  #################################################################
  ## calculate delay
  # get all service record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff)
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","VEH_SOLD_DT","MILEAGE_OUT")]
  temp <- na.omit(temp)
  # unique service visits
  temp <- temp[!duplicated(temp[,c("VIN_NO","JOB_ORD_NO")]),]
  # calculate service time in days
  temp$timing <- as.numeric(temp$JOB_ORD_DT-temp$VEH_SOLD_DT)
  # remove pre-sales cases
  temp <- subset(temp,temp$timing>=20)
  # define customer type, heavy usage 1 or light usage 0, annual mileage >20k
  temp$type <- temp$MILEAGE_OUT/temp$timing*365
  temp$type <- ifelse(temp$type>=20000,1,0)
  
  # calculate service interval
  temp <- temp[order(temp$VIN_NO,temp$JOB_ORD_DT),]
  # calculate service interval
  temp$INV_timing <- ave(temp$timing, temp$VIN_NO, FUN = function(x) c(0, diff(x)))
  temp$INV_mileage <- ave(temp$MILEAGE_OUT, temp$VIN_NO, FUN = function(x) c(0, diff(x)))
  # define delay as service interval >10k and service timing >6month
  temp$delay <- ifelse(temp$INV_timing>6*30&temp$INV_mileage>10000,1,0)
  # calculate mileage delay in days
  temp$delay_mileage <- as.integer(temp$INV_timing-temp$INV_timing/temp$INV_mileage*10000)
  # calculate timing delay indays
  temp$delay_timing <- temp$INV_timing-6*30
  temp$delay_mileage <- ifelse(temp$delay_mileage>=0, temp$delay_mileage,0)
  temp$delay_timing <- ifelse(temp$delay_timing>=0, temp$delay_timing,0)
  temp$delay_mileage[is.na(temp$delay_mileage)]=0
  temp$delay_timing[is.na(temp$delay_timing)]=0
  # define overall delay as the longer delay between mileage and timing (strict)
  temp$delay_all <- ifelse(temp$delay_timing>temp$delay_mileage,temp$delay_timing,temp$delay_mileage)

  #################################################################
  ## calculate every delay for each cust
  temp2 <- aggregate(delay_all~VIN_NO, data = temp, FUN = mean)
  cust.target$delay_avg <- temp2$delay_all[match(cust.target$VIN_NO,temp2$VIN_NO)]
  
  #################################################################
  ## calculate delay for each cust in last year
  # select record in last year
  temp3 <- subset(temp,temp$JOB_ORD_DT<=cutoff&temp$JOB_ORD_DT>cutoff-365)
  temp4 <- aggregate(delay_all~VIN_NO, data = temp3, FUN = mean)
  cust.target$delay_last <- temp4$delay_all[match(cust.target$VIN_NO,temp4$VIN_NO)]
  
  #################################################################
  ## calculate delay trend
  cust.target$delay_trend <- cust.target$delay_last/cust.target$delay_avg
  cust.target$delay_trend[is.na(cust.target$delay_trend)]=1
  
  return(cust.target)
  
  # plot data
  # temp2=subset(temp,temp$MILEAGE_OUT<=100000)
  # temp2=subset(temp2,temp2$timing<=60*30)
  # ggplot(temp2,aes(MILEAGE_OUT,timing/30)) + 
  #   geom_point(alpha = 0.1) +
  #   scale_x_continuous(breaks=seq(0,100000, by=10000)) +
  #   scale_y_continuous(breaks=seq(0,60, by=6)) +
  #   xlab("Milage During Service (K)")+
  #   ylab("Time Since Purchase (Month)") +
  #   theme_bw()
}