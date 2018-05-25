rm(list = ls())
# load library
library(plyr)
library(data.table)

# get work directory
wd=getwd();
# load function
source(file.path(wd,"functions","DateProcess.R",fsep="/"))
source(file.path(wd,"functions","DateProcess2.R",fsep="/"))
source(file.path(wd,"functions","TimeProcess.R",fsep="/"))
# for active
source(file.path(wd,"functions","TargetCustomers2.R",fsep="/"))
source(file.path(wd,"functions","FeatureDemo.R",fsep="/"))
source(file.path(wd,"functions","FeatureDrive.R",fsep="/"))
source(file.path(wd,"functions","FeatureFrequency.R",fsep="/"))
source(file.path(wd,"functions","FeatureRecency.R",fsep="/"))
source(file.path(wd,"functions","FeatureMoney.R",fsep="/"))
source(file.path(wd,"functions","FeatureDelay.R",fsep="/"))
source(file.path(wd,"functions","Featureparts.R",fsep="/"))
source(file.path(wd,"functions","FeatureDuration.R",fsep="/"))
source(file.path(wd,"functions","FeatureComplain.R",fsep="/"))
source(file.path(wd,"functions","FeatureComplain2.R",fsep="/"))

#############################################################
## set parameters
dealer.name <- "11155"
branch.name <- "HO"
# for active customer
cutoff <- as.Date("2017-04-01")

#############################################################
## load in data
file.list <- list.files("../4. Data/raw data/GS", pattern = "GS")
data.raw = do.call(rbind.fill, lapply(file.path("../4. Data/raw data/GS/",file.list,fsep = ""), function(x) fread(x, encoding = "UTF-8")))

# Select dealer data
data.raw <- setDT(data.raw)
data.all <- data.raw[DLR_CD == dealer.name & BRC_CD == branch.name,]
data.all <- as.data.frame(data.all)

# remove duplicates
data.all <- data.all[!duplicated(data.all),]

# process service date
data.all <- DateProcess(data.all,'JOB_ORD_DT')
data.all <- DateProcess(data.all,'VEH_SOLD_DT')
data.all <- DateProcess(data.all,'CUST_DOB')
data.all <- DateProcess(data.all,'ACT_RPR_START_DT')
data.all <- DateProcess(data.all,'ACT_RPR_END_DT')
data.all <- TimeProcess(data.all,'RPR_START_TM')
data.all <- TimeProcess(data.all,'RPR_END_TM')

# modify colnames
colnm <- names(data.all)
colnm <- gsub(" ", ".", colnm)
colnames(data.all) <- colnm

#############################################################
## customer filtering
output <- TargetCustomers2(data.all, dealer.name, branch.name,cutoff)
# unique target customer list
cust.target <- output$cust.target
# raw data,unique service records, dealer, non-fleet, na.omit
visits.all <- output$visits.all

# # save target customers data
# data.target <- subset(data.all,data.all$VIN_NO%in%cust.target$VIN_NO)
# fwrite(data.target, paste("../4. Data/Processed data/",dealer.name,"_target customer data.csv",sep = ""),row.names = FALSE)

#############################################################
## feature construction
# demo
cust.target <- FeatureDemo(data.all,visits.all,cust.target,cutoff)

# driving bahavior
cust.target <- FeatureDrive(data.all,cust.target,cutoff)

# frequency
cust.target <- FeatureFrequency(visits.all,cust.target,cutoff)

# recency & interval
cust.target <- FeatureRecency(visits.all,cust.target,cutoff)

# monetary
cust.target <- FeatureMoney(data.all,cust.target,cutoff)

# delay
cust.target <- FeatureDelay(data.all,cust.target,cutoff)

# parts
cust.target <- Featureparts(data.all,cust.target,cutoff)

# service time
cust.target <- FeatureDuration(data.all,cust.target,cutoff)

#############################################################
## write the data
write.csv(cust.target,paste("../4. Data/Processed data/",dealer.name,"_target customer data.csv",sep = ""),row.names = FALSE)
