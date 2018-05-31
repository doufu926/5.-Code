# extract discount features
FeatureDiscount <- function(data.all,visits.all,cust.target,cutoff){
  #############################################################
  ## % of visits with discount
  # get all service record before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff)
  # select the useful columns
  temp <- temp[,c("VIN_NO","JOB_ORD_NO","JOB_ORD_DT","Operation.Disc..Type","Part.Disc..Type","Outside.work.disc.prct")]
  # mark visits with discount
  temp$discount <- ifelse(temp$Operation.Disc..Type==""&temp$Part.Disc..Type==""&is.na(temp$Outside.work.disc.prct),0,1)
  # aggregate based on visit
  temp2 <- aggregate(discount~VIN_NO+JOB_ORD_NO, data = temp, sum)
  # with or without disc
  temp2$disc <- ifelse(temp2$discount>0,1,0)
  temp2 <- subset(temp2,temp2$JOB_ORD_NO%in%visits.all$JOB_ORD_NO)
  # count number of visits with disc
  temp3 <- aggregate(disc~VIN_NO, data = temp2, sum)
  cust.target$visit_disc <- temp3$disc[match(cust.target$VIN_NO,temp3$VIN_NO)]
  # percentage
  cust.target$visit_disc_p <- cust.target$visit_disc/cust.target$visit
  
  return(cust.target)
}