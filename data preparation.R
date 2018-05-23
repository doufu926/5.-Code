rm(list = ls())
# load library
library(plyr)

# get work directory
wd=getwd();
# load function
source(file.path(wd,"functions","DateProcess.R",fsep="/"))
source(file.path(wd,"functions","DateProcess2.R",fsep="/"))
source(file.path(wd,"functions","TimeProcess.R",fsep="/"))
# for active
source(file.path(wd,"functions","TargetCustomers.R",fsep="/"))
# # for inactive
# source(file.path(wd,"functions","TargetCustomers2.R",fsep="/"))
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
dealer.name <- "11304"
branch.name <- "HO"
# for active customer
cutoff <- as.Date("2016-06-30")
# for inactive customer
# cutoff <- as.Date("2015-06-30")

#############################################################
## load in data
file.list <- list.files("../4. Data/raw data/GS", pattern = "GS")
data.all = do.call(rbind.fill, lapply(file.path("../1. raw data/data/11304_GS_2012_2017/",file.list,fsep = ""), function(x) read.table(x, header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE, encoding = "UTF-8")))

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

#############################################################
## customer filtering
output <- TargetCustomers(data.all, dealer.name, branch.name,cutoff)
# unique target customer list
cust.target <- output$cust.target
# raw data,unique service records, dealer, non-fleet, na.omit
visits.all <- output$visits.all

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

# location
cust.target <- FeatureLocation(data.all,cust.target)

#############################################################
## load in complain data
require(XLConnect)
# load workbook
wb <- loadWorkbook("../1. raw data/data/VOC Complaint TSB Jan12-June17 v2.xlsx")
# load all worksheet
lst = readWorksheet(wb, sheet = getSheets(wb))
# combine into one dataframe
for (i in 1:length(lst)){
  if (i==1){
    complaint.all <- lst[[i]]
  }else{
    complaint.all <- rbind(complaint.all,lst[[i]])
  }
}

# process date
complaint.all$Open.Date=as.Date(complaint.all$Open.Date)
complaint.all$Incident..Date=as.Date(complaint.all$Incident..Date)
complaint.all$Close.DT=as.Date(complaint.all$Close.DT)
complaint.all=DateProcess2(complaint.all,'Summary.DT')
#############################################################
## feature construction from complaint data
# in entire ownership 
cust.target <- FeatureComplain(complaint.all,cust.target,cutoff)

# recency of complaint
cust.target <- FeatureComplain2(visits.all,complaint.all,cust.target,cutoff)

# deal with missing values
cust.target$age[is.na(cust.target$age)]=mean(cust.target$age,na.rm = TRUE)
cust.target$complain_time[is.na(cust.target$complain_time)]=0
cust.target$complain_visit[is.na(cust.target$complain_visit)]=0
# # for inactive customer only
# cust.target$delay_last[is.na(cust.target$delay_last)]=0
# cust.target$part_last[is.na(cust.target$part_last)]=0
# cust.target$duration_last[is.na(cust.target$duration_last)]=0


#############################################################
## write the data
write.csv(cust.target,"cust.target.location.csv",row.names = FALSE)
