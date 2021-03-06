# rm(list = ls())
# load library
library(ggplot2)
library(dplyr)
library(treemap)
library(data.table)
library(plyr)

# get work directory
wd=getwd();

# load function
source(file.path(wd,"functions","SalesService.R",fsep="/"))
source(file.path(wd,"functions","TotalCost.R",fsep="/"))
#############################################################
## set parameters
dealer.name <- "12021"
branch.name <- "HO"
# for active customer
cutoff <- as.Date("2017-04-01")

#############################################################
## load in data
# cust.target <- read.csv(paste("../4. Data/Processed data/",dealer.name,"_target customer data.csv",sep = ""))

file.list <- list.files("../4. Data/Processed data", pattern = "target customer data")
cust.target = do.call(rbind.fill, lapply(file.path("../4. Data/Processed data/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))


# make target factor
cust.target$retain <- as.factor(ifelse(cust.target$retain==0,"Churned","Retained"))

# load in prediction data
# cust.target2 <-  read.csv(paste("../4. Data/Processed data/","test_data_results_",dealer.name,".csv",sep = ""))

file.list <- list.files("../4. Data/Processed data", pattern = "test_data_results")
cust.target2 = do.call(rbind.fill, lapply(file.path("../4. Data/Processed data/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))

# map prediction results
cust.target$risk <- cust.target2$Risk[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$pred <- cust.target2$Predict[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target <- subset(cust.target,!is.na(cust.target$risk))
cust.target$cost_last_labor <- cust.target2$LAST_LABOR_COST[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$cost_last_parts <- cust.target2$LAST_TOTAL_COST[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$cost_last_parts <- cust.target$cost_last_parts-cust.target$cost_last_labor

# get sales and service features
output <- SalesService(cust.target,cutoff)
cust.target <- output$cust.target
service.all <- output$service.all
cust.target$visit_DLR_no_other <- cust.target$visit_DLR_no-1
cust.target$visit_BRC_no_other <- cust.target$visit_BRC_no-1
cust.target$visit_other <- cust.target$visit_total-cust.target$visit_total_current
cust.target$visit_other_nonsales <- cust.target$visit_other-cust.target$visit_total_sales
cust.target$visit_DLR_no_nonsales <- ifelse(cust.target$visit_total_sales>0,cust.target$visit_DLR_no_other-1,cust.target$visit_DLR_no_other)

# define segment
cust.target$segment <- cut(cust.target$risk,breaks = c(0,0.3,0.7,1),labels = c("Low","Mid","High"))
cust.target$segment2 <- cut(cust.target$risk,breaks = seq(0,1,0.1),labels = seq(1,10))
cust.target$segment3 <- with(cust.target, cut(risk, 
                                              breaks=quantile(risk, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                              include.lowest=TRUE,labels = seq(1,10)))
#############################################################
## calculate segmentation attributes
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


# #############################################################
# ## calculate segment size
# library(dplyr)
# segment.table <- cust.target %>%
#   group_by(segment,PM, loyalty,interval,visit_last_group) %>%
#   dplyr::summarize(n = n())
# 
# #############################################################
# ## plot treemap 
# # for high risk group
# segment.table.high <- subset(segment.table,segment.table$segment=="High")
# treemap(segment.table.high,index = c("PM","loyalty","interval","visit_last_group"),vSize = "n",type = "index",
#         fontsize.labels=c(15,12,10,8),
#         fontcolor.labels=c("white","grey","black","orange"),
#         bg.labels=c("transparent"),
#         align.labels=list(
#           c("left", "top"), 
#           c("center", "center"),
#           c("right", "bottom"),
#           c("left", "bottom")
#         ), 
#         overlap.labels=0.5,
#         inflate.labels=F)

# #############################################################
# ## detailed analysis for one time customers
# cust.1time <- subset(cust.target,cust.target$loyalty=="One Time")
# 
# # what service customer come for?
# temp=cust.1time[cust.1time$segment=="High",]
# plot(table(temp$last_PM))
# b=as.data.frame(table(temp$last_PM))
# 
# # where one time customer purchase from
# purchase.dlr <- cust.target %>%
#   select(
#          loyalty,
#          same_DLR) %>%
#   group_by(loyalty) %>%
#   dplyr::summarise(same_DLR=1-mean(same_DLR),
#                    count=n())
# 
# # for each dealer which dealer they purchase from
# dealer.name <- "15099"
# dealer.1time <- subset(cust.1time,cust.1time$Service_DLR==dealer.name)
# b=as.data.frame(table(dealer.1time$Sales_DLR))
# ## write the data
# write.csv(b,paste("../4. Data/Processed data/",dealer.name,"_purchase DLR.csv",sep = ""),row.names = FALSE)
# 
# # for customer purchase from other DLR and only come recently
# # where they doing service previsouly (remove case from first DLR)
# temp <- subset(cust.target,cust.target$same_DLR==0&
#                       cust.target$loyalty=="One Time"&
#                  cust.target$visit_total_current==1&
#                  cust.target$ownership<=5*365) #only analyze 5 year customers, to avoid used car cases
# 
# # for each dealer which dealer they service from
# dealer.name <- "15099"
# dealer.1time <- subset(temp,temp$Service_DLR==dealer.name)
# # get all the service record
# dealer.service <- subset(service.all,service.all$VIN_NO%in%dealer.1time$VIN_NO&service.all$JOB_ORD_DT<cutoff)
# dealer.service$DLR <- ifelse(dealer.service$DLR==dealer.service$sale_DLR,"Sales DLR",ifelse(dealer.service$DLR==dealer.service$current_DLR,"Pilot DLR","Other Toyota DLR"))
# b=as.data.frame.matrix(table(dealer.service$DLR,dealer.service$PERIOD))
# ## write the data
# write.csv(b,paste("../4. Data/Processed data/",dealer.name,"_service DLR.csv",sep = ""))
# 
# # calculate service potential
# temp$potential <- round(temp$ownership/365*2)
# temp$potential <- pmax(temp$visit_total,temp$potential)
# 
# # how many of them visited original DLR
# temp$sales <- ifelse(temp$visit_total_sales>0,"Sales_Yes","Sales_No")
# # how many of them visited other DLR
# temp$other <- ifelse(temp$visit_DLR_no_nonsales>0,"OtherT_Yes","OtherT_No")
# # how many of them visited outside
# temp$outside <- ifelse(temp$potential>temp$visit_total,"Outside_Yes","Outside_No")
# 
# service.loc <- temp %>%
#   select(visit_other,
#          segment) %>%
#   group_by(segment) %>%
#   dplyr::summarise(mean(visit_other),
#                  n() )
# 
# temp2 <- subset(temp,temp$sales=="Sales_No"&
#                   temp$other=="OtherT_Yes")
# b=as.data.frame(table(temp2$visit_DLR_no_nonsales))



# #############################################################
# ## detailed analysis for non regular customers
# cust.nonregular <- subset(cust.target,cust.target$PM!="50K"
#                      & cust.target$loyalty!="One Time"
#                      & cust.target$interval=="Non-regular")
# 
# # calculate high risk customer percentage
# table(cust.nonregular$segment)
# 
# # select high risk customers
# temp=cust.nonregular[cust.nonregular$segment=="Low",]
# 
# # get profile
# profile <- cust.nonregular %>%
#   select(segment,
#          interval_mean,
#          interval_max,
#          visit_avg,
#          visit,
#          ownership,
#          visit_year,
#          mileage_day) %>%
#   group_by(segment) %>%
#   dplyr::summarise(interval_mean=median(interval_mean,na.rm = TRUE),
#                    interval_max=median(interval_max,na.rm = TRUE),
#                    visit_avg=median(visit_avg,na.rm = TRUE),
#                    median(visit,na.rm = TRUE),
#                    ownership=median(ownership,na.rm = TRUE),
#                    visit_year=median(visit_year,na.rm = TRUE),
#                    mileage_day=median(mileage_day,na.rm = TRUE),
#                    count=n())
# 
# profile <- temp %>%
#   select(interval,mileage_day) %>%
#   group_by(interval) %>%
#   dplyr::summarise(mileage_day=median(mileage_day,na.rm = TRUE))
# 
# # what service they come for?
# data.all <- as.data.frame(data.raw)
# 
# # process service date
# data.all <- DateProcess(data.all,'JOB_ORD_DT')
# data.all <- DateProcess(data.all,'VEH_SOLD_DT')
# 
# # modify colnames
# colnm <- names(data.all)
# colnm <- gsub(" ", ".", colnm)
# colnames(data.all) <- colnm
# 
# # remove non PM records
# data.all <- setDT(data.all)
# # did PM service
# data.all[, PM := ifelse(grepl("เช็คระยะ",OP_desc)==TRUE,1,0)]
# # find job has PM code
# data.all <- data.all[PM==1]
# data.all$PM <- NULL
# 
# # find unique visits
# data.all <- unique(data.all, by=c("VIN_NO","JOB_ORD_DT"))
# # find service time and mileage
# data.all[,PM:= as.numeric(OP_CD)/1000]
# data.all[,mileage:= as.numeric(MILEAGE_OUT)/1000]
# data.all[,time:= as.numeric(JOB_ORD_DT-VEH_SOLD_DT)/30]
# 
# # find all service record non regular customers
# data.nonregular <- subset(data.all,VIN_NO%in%temp$VIN_NO)
# temp2 <- subset(data.nonregular,mileage<=100&time<=60)
# # plot service pattern
# plot(temp2$mileage,temp2$time,col="#00000044",xlab="Milage During Service (K)",ylab="Time Since Purchase (Month)",main="Non-regular Timing Distribution")
# 
# # calculate all service interval
# temp2 <- subset(data.nonregular,JOB_ORD_DT<cutoff)
# temp2 <- temp2[order(VIN_NO,JOB_ORD_DT),]
# # calculate service interval since purchase
# temp2[,SRV_INT:= as.numeric(JOB_ORD_DT-VEH_SOLD_DT)]
# # filter customers service time <0 days from purchase, presales customers or revisit customers
# temp2 <- subset(temp2, SRV_INT>=0)
# # calculate service interval between two service
# temp2[,SRV_INT2:=ave(SRV_INT, VIN_NO, FUN = function(x) c(min(x), diff(x)))/365]
# # plot service pattern
# temp3 <- subset(temp2,PM<=100)
# plot(temp3$PM,temp3$SRV_INT2,col="#00000044",xlab="Milage During Service (K)",ylab="Time Since Purchase (Month)",main="Non-regular Timing Distribution")
# boxplot(SRV_INT2~PM,data = temp3,xlab="Service Conducted (PM Mileage)",ylab="Time Gap Since Last Service",outline=FALSE)
# 
# # calculate PM gap
# temp2[,PM_GAP:=ave(PM, VIN_NO, FUN = function(x) c(NA, diff(x)))]
# # remove na
# temp3 <- subset(temp2,!is.na(temp2$PM_GAP))
# temp3 <- subset(temp2,temp2$PM_GAP>=0)
# temp3$PM_GAP <- ifelse(temp3$PM_GAP%%10==9,temp3$PM_GAP+1,temp3$PM_GAP)
# plot(table(temp3$PM_GAP))
# b=as.data.frame(table(temp3$PM_GAP))
#############################################################
## detailed analysis for one visit customers
cust.1visit <- subset(cust.target,cust.target$PM!="50K"
                          & cust.target$loyalty!="One Time"
                          & cust.target$interval!="Non-regular"
                      &cust.target$visit_last_group=="1 visit")

cust.loyal <- subset(cust.target,cust.target$PM!="50K"
                      & cust.target$loyalty!="One Time"
                      & cust.target$interval!="Non-regular")

# select high risk customers
temp=cust.nonregular[cust.nonregular$segment=="High",]

# get profile
profile <- cust.loyal %>%
  select(visit_last_group,
cost_visit,
cost_visit_last) %>%
  group_by(visit_last_group) %>%
  dplyr::summarise(cost_visit=median(cost_visit),
                   cost_visit_last=median(cost_visit_last))

profile <- cust.loyal %>%
  select(segment,
         cost_visit,
         cost_visit_last) %>%
  dplyr::summarise(cost_visit=median(cost_visit),
                   cost_visit_last=median(cost_visit_last))

profile <- cust.1visit %>%
  select(
         last_PM,
         cost_visit,
         cost_visit_last,
         part_last,
         cost_last_parts) %>%
  group_by(last_PM) %>%
  dplyr::summarise(cost_visit=median(cost_visit),
                   cost_visit_last=median(cost_visit_last),
                   part_last=median(part_last),
                   cost_last_parts=median(cost_last_parts))

# get cost for each service visit
data.all <- as.data.frame(data.raw)

# process service date
data.all <- DateProcess(data.all,'JOB_ORD_DT')
data.all <- DateProcess(data.all,'VEH_SOLD_DT')

# modify colnames
colnm <- names(data.all)
colnm <- gsub(" ", ".", colnm)
colnames(data.all) <- colnm

# remove non PM records
data.all <- setDT(data.all)
# did PM service
data.all[, PM := ifelse(grepl("เช็คระยะ",OP_desc)==TRUE,1,0)]
temp <- data.all[,sum(PM),by=.(VIN_NO,JOB_ORD_NO)]
# find job has PM code
temp <- subset(temp,V1>0)
temp[,key:= paste(VIN_NO,JOB_ORD_NO)]
data.all[,key:= paste(VIN_NO,JOB_ORD_NO)]
data.all <- subset(data.all,key%in%temp$key)

# calculate cost
data.all=as.data.frame(data.all)
all.cost <- TotalCost(data.all)
data.all <- setDT(data.all)
temp <- data.all[,j=list(sum(PM),.N),by=list(VIN_NO,JOB_ORD_NO)]
temp[,PM_Only:=ifelse(V1==N,1,0)]
temp[all.cost,on = "JOB_ORD_NO"]
all.cost <- merge(all.cost,temp,by = c("VIN_NO","JOB_ORD_NO"))
temp <- data.all[PM==1]
temp[,PM:=as.numeric(OP_CD)/1000]
temp <- temp[,.(VIN_NO,JOB_ORD_NO,PM)]
all.cost <- merge(all.cost,temp,by = c("VIN_NO","JOB_ORD_NO"))
