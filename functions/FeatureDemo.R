# extract demographic features
FeatureDemo <- function(data.all,visits.all,cust.target,cutoff){
  #############################################################
  ## model
  # get model from raw data
  cust.target$model <- as.character(data.all$SRS_CD[match(cust.target$VIN_NO,data.all$VIN_NO)])
  cust.target$model[cust.target$model%in%c("HLBC","HLPV","HRBC","HLU2","HLU4","HLUP")]="Hilux"
  cust.target$model[cust.target$model%in%c("HLUD","HL4D","HR4D")]="Hilux-D"
  cust.target$model[cust.target$model%in%c("SONA","VIOS")]="Vios"
  cust.target$model[cust.target$model%in%c("","ALPH","VELL","CONA","WISH","CARA")]="Others"
  #############################################################
  ## ownership till cut off
  cust.target$ownership <- visits.all$VEH_SOLD_DT[match(cust.target$VIN_NO,visits.all$VIN_NO)]
  cust.target$ownership <- as.numeric(cutoff-cust.target$ownership)
  #############################################################  
  ## age till cutoff
  temp <- data.all[,c("VIN_NO","CUST_MARITAL_STS","CUST_DOB")]
  temp <- na.omit(temp)
  temp <- unique(temp)
  temp$age <- as.numeric(cutoff-temp$CUST_DOB)/365
  # remove outliers
  a <- summary(temp$age)
  lower.limit <- max((a[2]-1.5*(a[5]-a[2])),18)
  upper.limit <- a[5]+1.5*(a[5]-a[2])
  temp2 <- subset(temp,temp$age>=lower.limit&temp$age<=upper.limit)
  cust.target$age <- temp2$age[match(cust.target$VIN_NO,temp2$VIN_NO)]
  
  return(cust.target)
}