# feature for sales DLR & service DLR
library(RODBC)
library(readxl)
SalesService <- function(cust.target,cutoff){
  #############################################################
  ## load in data
  # sales
  sales <- fread("../4. Data/Raw data/Sales/4 Pilot dealers Enhance.txt",encoding = "UTF-8")
  
  # service
  service <- read_xlsx("../4. Data/Raw data/GS_other/service_other_DLR.xlsx")
  
  #############################################################
  ## sales DLR
  # get sales DLR for target customers
  cust.target$Sales_DLR <- sales$`Sale dealer code`[match(cust.target$VIN_NO,sales$`VIN NO`)]
  cust.target$Service_DLR <- sales$DLR[match(cust.target$VIN_NO,sales$`VIN NO`)]
  
  # same DLR?
  cust.target$same_DLR <- ifelse(cust.target$Service_DLR==cust.target$Sales_DLR,1,0)
  
  #############################################################
  ## service DLR
  # get all service record before cutoff
  service$JOB_ORD_DT <- as.Date(service$JOB_ORD_DT,"%Y-%m-%d")
  service <- subset(service, service$JOB_ORD_DT<cutoff)
  # get all PM service
  service <- subset(service, !is.na(service$PERIOD))
  # get current service DLR
  service$current_DLR <- cust.target$Service_DLR[match(service$VIN_NO,cust.target$VIN_NO)]
  service$current_BRC <- paste(service$current_DLR,"HO",sep = "")
  # get sales DLR
  service$sale_DLR <- cust.target$Sales_DLR[match(service$VIN_NO,cust.target$VIN_NO)]
  temp <- service[,c("VIN_NO","DLR","BranchID","JOB_ORD_DT")]
  # number of DLR visited for each vin
  temp1 <- temp[!duplicated(temp[,c("VIN_NO","DLR")]),]
  temp1 <- aggregate(DLR~VIN_NO, data = temp1, FUN = function(x) length(x))
  # number of branch visited
  temp2 <- temp[!duplicated(temp[,c("VIN_NO","BranchID")]),]
  temp2 <- aggregate(BranchID~VIN_NO, data = temp2, FUN = function(x) length(x))
  # number of visits in total
  temp3 <- temp[!duplicated(temp[,c("VIN_NO","JOB_ORD_DT")]),]
  temp3 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp3, FUN = function(x) length(x))
  # add feature to cust.target
  cust.target$visit_DLR_no <- temp1$DLR[match(cust.target$VIN_NO,temp1$VIN_NO)]
  cust.target$visit_BRC_no <- temp2$BranchID[match(cust.target$VIN_NO,temp2$VIN_NO)]
  cust.target$visit_total <- temp3$JOB_ORD_DT[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  # get total service number to current DLR
  temp <- subset(service,service$BranchID==service$current_BRC)
  temp <- temp[,c("VIN_NO","DLR","BranchID","JOB_ORD_DT")]
  temp3 <- temp[!duplicated(temp[,c("VIN_NO","JOB_ORD_DT")]),]
  temp3 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp3, FUN = function(x) length(x))
  cust.target$visit_total_current <- temp3$JOB_ORD_DT[match(cust.target$VIN_NO,temp3$VIN_NO)]
  
  # get total service number to sales DLR
  temp <- subset(service,service$DLR==service$sale_DLR&service$current_DLR!=service$sale_DLR)
  temp <- temp[,c("VIN_NO","DLR","BranchID","JOB_ORD_DT")]
  temp3 <- temp[!duplicated(temp[,c("VIN_NO","JOB_ORD_DT")]),]
  temp3 <- aggregate(JOB_ORD_DT~VIN_NO, data = temp3, FUN = function(x) length(x))
  cust.target$visit_total_sales <- temp3$JOB_ORD_DT[match(cust.target$VIN_NO,temp3$VIN_NO)]
  cust.target$visit_total_sales[is.na(cust.target$visit_total_sales)]=0
  
  # output
  output <- list("cust.target" = cust.target, "service.all" = service)
  return(output)
}