# monetary feature
source(file.path(wd,"functions","TotalCost.R",fsep="/"))
FeatureMoney <- function(data.all,cust.target,cutoff){
  #################################################################
  # total spending
  # select all transactions before cutoff
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff)
  # calculate total spending for each customer
  total.cost <- TotalCost(temp)
  # aggregate by vin number
  temp2 <- aggregate(total~VIN_NO, data = total.cost, sum)
  # assign to target custoemrs
  cust.target$cost_total <- temp2$total[match(cust.target$VIN_NO,temp2$VIN_NO)]
  
  #################################################################
  # total spending in last year
  # select all transactions in last year
  temp <- subset(data.all,data.all$JOB_ORD_DT<=cutoff&data.all$JOB_ORD_DT>cutoff-365)
  # calculate total spending for each customer
  total.cost <- TotalCost(temp)
  # aggregate by vin number
  temp2 <- aggregate(total~VIN_NO, data = total.cost, sum)
  # assign to target custoemrs
  cust.target$cost_last <- temp2$total[match(cust.target$VIN_NO,temp2$VIN_NO)]
  cust.target$cost_last[is.na(cust.target$cost_last)]=0
  
  #################################################################
  # spend/year
  cust.target$cost_avg <- ifelse(cust.target$visit_year<365,cust.target$cost_total,cust.target$cost_total/cust.target$visit_year*365)
  
  #################################################################
  # spend/visit
  cust.target$cost_visit <- cust.target$cost_total/cust.target$visit
  
  #################################################################
  # spend/visit last year
  cust.target$cost_visit_last <- cust.target$cost_last/cust.target$visit_last
  cust.target$cost_visit_last[is.na(cust.target$cost_visit_last)]=0
  cust.target$cost_visit_last[cust.target$cost_visit_last==Inf]=0
  
  #################################################################
  # spend/visit last year
  cust.target$cost_trend <- cust.target$cost_visit_last/cust.target$cost_visit
  
  return(cust.target)
}