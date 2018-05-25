# find target customers
# non-fleet, service at dealer, regular customer, retained in last year
TargetCustomers2 <- function(data.all,dealer.name,branch.name, cutoff){
  # did service at targeted dealer & branch
  data.target <- subset(data.all, data.all$DLR_CD==dealer.name)
  data.target <- subset(data.target, data.target$BRC_CD==branch.name)
  print(length(unique(data.target$VIN_NO)))
  
  # fitler customer serviced in last 12 months
  data.target$retain.before <- data.target$JOB_ORD_DT> cutoff-365 & data.target$JOB_ORD_DT<= cutoff
  temp <- aggregate(retain.before~VIN_NO+CUST_ID_NO, data = data.target, sum)
  # define next year retention status
  cust.target.unique <- unique(subset(temp,temp$retain.before>0))
  cust.target.unique$key <- paste(cust.target.unique$VIN_NO,cust.target.unique$CUST_ID_NO)
  data.target$key <- paste(data.target$VIN_NO,data.target$CUST_ID_NO)
  data.target <- subset(data.target,data.target$key%in%cust.target.unique$key)
  print(length(unique(data.target$VIN_NO)))
  
  # non-fleet customers
  data.target <- subset(data.target, data.target$CUST_ID_TYP=="C")
  print(length(unique(data.target$VIN_NO)))
  
  # find customer max service interval
  data.target <- unique(data.target[,c('VIN_NO','JOB_ORD_DT','VEH_SOLD_DT')])  # find unique service visits
  data.target <- na.omit(data.target)  # remove records with missing information
  data.target <- data.target[order(data.target$VIN_NO,data.target$JOB_ORD_DT),]  # sort records by vin and service date
  print(length(unique(data.target$VIN_NO)))
  
  # filter customers serviced before cut off date, and define regular customer
  cust.target <- subset(data.target, data.target$JOB_ORD_DT<= cutoff)
  print(length(unique(cust.target$VIN_NO)))
  # cust.target <- data.target
  # calculate service interval since purchase
  cust.target$SRV_INT <- as.numeric(cust.target$JOB_ORD_DT-cust.target$VEH_SOLD_DT)
  # filter customers service time <20 days from purchase, presales customers or revisit customers
  cust.target <- subset(cust.target, cust.target$SRV_INT>20)
  print(length(unique(cust.target$VIN_NO)))
  # calculate service interval between two service
  cust.target$SRV_INT2 <- ave(cust.target$SRV_INT, cust.target$VIN_NO, FUN = function(x) c(min(x), diff(x)))
  cust.target$SRV_INT3 <- ave(cust.target$SRV_INT, cust.target$VIN_NO, FUN = function(x) c(0, diff(x)))
  # calculate max interval, if max interval is the first service, find the next largest (newly retained customer)
  temp1 <- aggregate(SRV_INT2~VIN_NO, data=cust.target, max)
  temp2 <- aggregate(SRV_INT3~VIN_NO, data=cust.target, max) 
  temp2$SRV_INT3[temp2$SRV_INT3==0] <- Inf
  temp2$SRV_INT2 <- temp1$SRV_INT2
  temp2$SRV_INT_MAX <- apply(temp2[,c('SRV_INT3','SRV_INT2')],1,min)
  cust.target$SRV_INT_MAX <- temp2$SRV_INT_MAX[match(cust.target$VIN_NO,temp2$VIN_NO)]  # assign to each record
  # # filter customers max interval >15 month, non-loyal/regular customer until cut off date???
  # cust.target <- subset(cust.target, cust.target$SRV_INT_MAX<=15*30)
  
  # get all service record for those customers including results
  cust.target <- subset(data.target, data.target$VIN_NO%in%cust.target$VIN_NO)
  # fitler customer serviced in last 12 months
  cust.target$retain.before <- cust.target$JOB_ORD_DT> cutoff-365 & cust.target$JOB_ORD_DT<= cutoff
  cust.target$retain <- cust.target$JOB_ORD_DT> cutoff & cust.target$JOB_ORD_DT<= cutoff+365
  temp1 <- aggregate(retain.before~VIN_NO, data = cust.target, sum)
  temp2 <- aggregate(retain~VIN_NO, data = cust.target, sum)
  temp2$retain.before <- temp1$retain.before[match(temp2$VIN_NO,temp1$VIN_NO)]
  # define next year retention status
  cust.target.unique <- unique(subset(temp2,temp2$retain.before>0))
  cust.target.unique$retain[cust.target.unique$retain>0] <- 1
  cust.target.unique$retain.before=NULL
  print(sum(cust.target.unique$retain))
  
  # output
  output <- list("cust.target" = cust.target.unique, "visits.all" = data.target)
  return(output)
}