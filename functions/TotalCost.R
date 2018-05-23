# total service cost
TotalCost <- function(data.all){
  #################################################################
  # total labor cost
  temp <- unique(data.all[,c("VIN_NO","JOB_ORD_NO","OP_CD","Invoice.Labor.amount")])
  labor.cost <- aggregate(Invoice.Labor.amount~VIN_NO+JOB_ORD_NO, data = temp, sum)
  #################################################################
  # total parts cost
  temp <- unique(data.all[,c("VIN_NO","JOB_ORD_NO","PART_NO","Part.Issue.Qty","Part.Invoice.amount")])
  temp <- na.omit(temp)
  temp$cost <- temp$Part.Issue.Qty*temp$Part.Invoice.amount
  part.cost <- aggregate(cost~VIN_NO+JOB_ORD_NO, data = temp, sum)
  #################################################################
  # total outside cost
  temp <- unique(data.all[,c("VIN_NO","JOB_ORD_NO","OUT_OP_CD","Outside.work.invoice.amount")])
  temp <- na.omit(temp)
  out.cost <- aggregate(Outside.work.invoice.amount~VIN_NO+JOB_ORD_NO, data = temp, sum)
  #################################################################
  # total cost with tax
  all.cost <- merge(labor.cost,part.cost,by=c("VIN_NO","JOB_ORD_NO"),all = TRUE)
  all.cost <- merge(all.cost,out.cost,by=c("VIN_NO","JOB_ORD_NO"),all = TRUE)
  all.cost[is.na(all.cost)]=0
  all.cost$total <- all.cost$Invoice.Labor.amount+all.cost$cost+all.cost$Outside.work.invoice.amount
  all.cost$total <- all.cost$total*1.07
  
  return(all.cost)
}