rm(list = ls())
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
cust.target <- read.csv(paste("../4. Data/Processed data/",dealer.name,"_target customer data.csv",sep = ""))
# make target factor
cust.target$retain <- as.factor(ifelse(cust.target$retain==0,"Churned","Retained"))

# load in prediction data
cust.target2 <-  read.csv(paste("../4. Data/Processed data/","test_data_results_",dealer.name,".csv",sep = ""))

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

# 
# #############################################################
# ## calculate segmentation attributes
# # last PM
# # 1: <50k, 2: =50k, 3: >50k
# cust.target$PM <- ifelse(cust.target$last_PM==50,2,ifelse(cust.target$last_PM<50,1,3))
# 
# # loyalty
# # 1: always visit DLR since purchase, 2: newly retained, visited more than 2 times, 3: newly retained, 1 time
# cust.target$loyalty <- ifelse(cust.target$loyal==1,1,ifelse(cust.target$visit>=2,2,3))
# 
# # # service interval
# # # 1: mean service interval < 12 2: mean service interval >12
# # cust.target$interval <- ifelse(cust.target$interval_mean<=365,1,2)
# 
# # service max interval
# # 1: mean service interval < 12 2: mean service interval >12
# cust.target$interval <- ifelse(cust.target$interval_max<=365,1,2)

#############################################################
## calculate segmentation attributes
# last PM
# 1: <50k, 2: =50k, 3: >50k
cust.target$PM <- ifelse(cust.target$last_PM==50,"50K",ifelse(cust.target$last_PM<50,"In warranty","Out Warranty"))

# loyalty
# 1: always visit DLR since purchase, 2: newly retained, visited more than 2 times, 3: newly retained, 1 time
cust.target$loyalty <- ifelse(cust.target$loyal==1,"Loyal",ifelse(cust.target$visit>=2,"New Loyal","One Time"))

# service max interval
# 1: mean service interval < 12 2: mean service interval >12
cust.target$interval <- ifelse(cust.target$interval_max<=365,"Regular","Non-regular")

# # cost during last service
# # 1: cost <= average 2: cost >average
# cust.target$cost_group <- ave(cust.target$cost_visit_last, cust.target$PM, FUN=median)
# cust.target$cost <- ifelse(cust.target$cost_trend<1,"Reducing cost","Maintain cost")

# # mileage/day
# # 1: low than average 2: higher than average
# cust.target$mileage_group <- ave(cust.target$mileage_day, cust.target[,c("PM","loyalty","interval")], FUN=mean)
# cust.target$mileage_high <- ifelse(cust.target$mileage_day<=cust.target$mileage_group,"Low Mileage","High Mileage")

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
