# rm(list = ls())
# load library
library(ggplot2)
library(dplyr)
library(treemap)

# get work directory
wd=getwd();

# load function

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
# cust.target$interval_max <- cust.target2$max_interval[match(cust.target$VIN_NO,cust.target2$VIN_NO)]

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


#############################################################
## calculate segment size
library(dplyr)
segment.table <- cust.target %>%
  group_by(segment,PM, loyalty,interval,visit_last_group) %>%
  dplyr::summarize(n = n())

#############################################################
## plot treemap 
# for high risk group
segment.table.high <- subset(segment.table,segment.table$segment=="High")
treemap(segment.table.high,index = c("PM","loyalty","interval","visit_last_group"),vSize = "n",type = "index",
        fontsize.labels=c(15,12,10,8),
        fontcolor.labels=c("white","grey","black","orange"),
        bg.labels=c("transparent"),
        align.labels=list(
          c("left", "top"), 
          c("center", "center"),
          c("right", "bottom"),
          c("left", "bottom")
        ), 
        overlap.labels=0.5,
        inflate.labels=F)

#############################################################
## detailed analysis for one time customers
cust.1time <- subset(cust.target,cust.target$PM!="50K" 
                     & cust.target$loyalty=="One Time")

# what service customer come for?
temp=cust.1time[cust.1time$segment=="High",]
plot(table(temp$last_PM))
b=as.data.frame(table(temp$last_PM))
#############################################################
## detailed analysis for non regular customers
cust.nonregular <- subset(cust.target,cust.target$PM!="50K" 
                     & cust.target$loyalty!="One Time" 
                     & cust.target$interval=="Non-regular"
                     & cust.target$visit>1)

# calculate high risk customer percentage
table(cust.nonregular$segment)

# select high risk customers
temp=cust.nonregular[cust.nonregular$segment=="High",]

# get profile
profile <- cust.nonregular %>%
  select(segment,
         interval_mean,
         interval_max,
         visit_avg,
         ownership,
         visit_year) %>%
  group_by(segment) %>%
  dplyr::summarise(interval_mean=median(interval_mean,na.rm = TRUE),
                   interval_max=median(interval_max,na.rm = TRUE),
                   visit_avg=median(visit_avg,na.rm = TRUE),
                   ownership=median(ownership,na.rm = TRUE),
                   visit_year=median(visit_year,na.rm = TRUE),
                   count=n())

# what service they come for?
# temp2 <- subset(data.all,data.all$JOB_ORD_DT<cutoff)
# temp2 <- subset(temp2,temp2$PM==1)
# # find all service record non regular customers
# temp2 <- temp2[,c("VIN_NO","JOB_ORD_DT","OP_CD","VEH_SOLD_DT","MILEAGE_OUT")]
# temp2 <- temp2[!duplicated(temp2[,c("VIN_NO","JOB_ORD_DT")]),]
# temp2$PM <- as.numeric(temp2$OP_CD)/1000
# temp2$TM <- as.numeric(temp2$JOB_ORD_DT-temp2$VEH_SOLD_DT)/30
# temp2$mileage <- temp2$MILEAGE_OUT/1000
# temp2 <- subset(temp2,temp2$VIN_NO%in%temp$VIN_NO)
# # plot service pattern
# temp2 <- subset(temp2,temp2$mileage<=100&temp2$TM<=60)
# plot(temp2$mileage,temp2$TM,col="#00000044",xlab="Milage During Service (K)",ylab="Time Since Purchase (Month)",main="Non-regular Timing Distribution")

data.all <- as.data.frame(data.all)

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
data.all[, PM := ifelse(grepl("เช็คระยะ",OP_desc)==TRUE,0,1)]
# aggregate by job
temp <- data.all[,PM:= sum(PM), by=.(VIN_NO,JOB_ORD_NO)]
# find job has PM code
temp <- temp[,]
temp$PM <- NULL
temp$key <- paste(temp$VIN_NO,temp$JOB_ORD_NO)
data.all$key <- paste(data.all$VIN_NO,data.all$JOB_ORD_NO)
data.all <- subset(data.all,data.all$key%in%temp$key)