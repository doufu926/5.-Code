rm(list = ls())
# load library
library(dplyr)
library(data.table)
library(plyr)

# get work directory
wd=getwd()

# load function

#############################################################
## load in data for all DLR
# final voc list
file.list <- list.files("../4. Data/Processed data/target customer final", pattern = ".txt")
voc.list = do.call(rbind.fill, lapply(file.path("../4. Data/Processed data/target customer final/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))

# target customer
file.list <- list.files("../4. Data/Processed data", pattern = "target customer data")
cust.target = do.call(rbind.fill, lapply(file.path("../4. Data/Processed data/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))
# make target factor
cust.target$retain <- as.factor(ifelse(cust.target$retain==0,"Churned","Retained"))

# load in prediction data
file.list <- list.files("../4. Data/Processed data", pattern = "test_data_results")
cust.target2 = do.call(rbind.fill, lapply(file.path("../4. Data/Processed data/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))

#############################################################
## define segment
# map prediction results
cust.target$risk <- cust.target2$Risk[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$pred <- cust.target2$Predict[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target <- subset(cust.target,!is.na(cust.target$risk))
rm(cust.target2)
gc()

# risk segments
cust.target$segment3 <- cut(cust.target$risk,breaks = c(0,0.3,0.7,1),labels = c("Low","Mid","High"))

# defind 6 segments
cust.target$segment6 <- ifelse(cust.target$segment3=="Low"&cust.target$retain=="Retained","LR",
                               ifelse(cust.target$segment3=="Low"&cust.target$retain=="Churned","LC",
                                      ifelse(cust.target$segment3=="Mid"&cust.target$retain=="Retained","MR",
                                             ifelse(cust.target$segment3=="Mid"&cust.target$retain=="Churned","MC",
                                                    ifelse(cust.target$segment3=="High"&cust.target$retain=="Retained","HR","HC"      
                                             )))))
#############################################################
## defind high risk profiles
# last PM
# 1: <50k, 2: =50k, 3: >50k
cust.target$PM <- ifelse(cust.target$last_PM==50,"50K",ifelse(cust.target$last_PM<50,"In warranty","Out Warranty"))

# loyalty
# 1: always visit DLR since purchase, 2: newly retained, visited more than 2 times, 3: newly retained, 1 time
cust.target$loyalty <- ifelse(cust.target$loyal==1,"Loyal",ifelse(cust.target$visit>=2,"New Loyal","One Time"))
# cust.target$loyalty <- ifelse(cust.target$visit==1,"One Time",ifelse(cust.target$loyal!=1,"New Loyal","Loyal"))

# service max interval
# 1: mean service interval < 12 2: mean service interval >12
cust.target$interval <- ifelse(cust.target$interval_max<=365,"Regular","Non-regular")

# visit in last year
# 1: 1 time 2: more than 1 time
cust.target$visit_last_group <- ifelse(cust.target$visit_last==1,"1 visit",">1 visit")

# defind high risk profiles
cust.target$high_risk_profile <- ifelse(cust.target$segment3!="High",NA,
                                        ifelse(cust.target$PM=="50K","HC1",
                                               ifelse(cust.target$loyalty=="One Time","HC2",
                                                      ifelse(cust.target$interval=="Non-regular","HC3",
                                                             ifelse(cust.target$visit_last_group=="1 visit","HC4","Others"
                                                                    )))))

#############################################################
## defind final segment
cust.target$segment_final <- ifelse(cust.target$segment6=="HC",cust.target$high_risk_profile,cust.target$segment6)

#############################################################
## map to final list
voc.list$Churn_Actual <- cust.target$retain[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Churn_Pred <- cust.target$pred[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Churn_Pred <- as.factor(ifelse(voc.list$Churn_Pred==1,"Churned","Retained"))
voc.list$Risk <- cust.target$risk[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Segment_3 <- cust.target$segment3[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Segment_6 <- cust.target$segment6[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Profile_4 <- cust.target$high_risk_profile[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list$Segment_Final <- cust.target$segment_final[match(voc.list$`VIN NO`,cust.target$VIN_NO)]
voc.list <- subset(voc.list,!is.na(voc.list$Risk))
voc.list <- subset(voc.list,voc.list$Segment_Final!="Others")

#############################################################
## quota calculation
count <- as.data.frame.matrix(table(voc.list$DLR,voc.list$Segment_Final))
count$HC <- apply(count[,1:4],1,sum)
ratio_dlr <- as.data.frame(prop.table(as.matrix(count),2))
ratio_profile <- as.data.frame(prop.table(as.matrix(count[,1:4]),1))
# set quota
quota <- count
quota[,c("LR","MR","MC","HC")]=30
# consolidated sample for LC, HR
quota$LC <- round(ratio_dlr$LC*30)
quota$HR <- round(ratio_dlr$HR*30)
# Profile quota based on DLR quota
quota[,1:4] <- round(ratio_profile[,1:4]*30)
# Profile quota based on prfile quota
quota2 <- quota
quota2[,1:4] <- round(ratio_dlr[,1:4]*30)
quota_final <- pmax(quota,quota2)
