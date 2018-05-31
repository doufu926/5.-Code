rm(list = ls())
# load library
library(ggplot2)
library(dplyr)

# get work directory
wd=getwd();

# load function

#############################################################
## set parameters
dealer.name <- "15099"
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

# add in features
cust.target$wait <- cust.target2$wait_flg...[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$wash <- cust.target2$wash_flg...[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
cust.target$last_PM <- cust.target2$last_PM[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
# define segment
cust.target$segment <- cut(cust.target$risk,breaks = c(0,0.3,0.7,1),labels = c("Low","Mid","High"))
cust.target$segment2 <- cut(cust.target$risk,breaks = seq(0,1,0.1),labels = seq(1,10))
cust.target$segment3 <- with(cust.target, cut(risk, 
                                breaks=quantile(risk, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                include.lowest=TRUE,labels = seq(1,10)))
#############################################################
## segment profile - aggregate
# # 2 segment
# profile <- cust.target %>%
#   select(retain,
#          age,
#          ownership,
#          visit_year,
#          warranty,
#          warranty_labor,
#          duration_avg,
#          duration_last,
#          milage,
#          mileage_day,
#          visit_avg,
#          visit_last,
#          visit_trend,
#          recency,
#          interval_mean,
#          delay_last,
#          delay_avg,
#          delay_trend,
#          cost_total,
#          cost_trend,
#          cost_avg,
#          cost_visit,
#          part_avg,
#          part_last) %>%
#   group_by(retain) %>%
#   dplyr::summarise(age = median(age,na.rm = TRUE), 
#                    ownership = median(ownership),
#                    visit_year=median(visit_year),
#                    warranty=sum(warranty),
#                    warranty_labor=sum(warranty_labor),
#                    duration_avg=median(duration_avg),
#                    duration_last=median(duration_last),
#                    milage=median(milage),
#                    mileage_day=median(mileage_day),
#                    visit_avg=median(visit_avg),
#                    visit_last=median(visit_last),
#                    visit_trend=median(visit_trend),
#                    recency=median(recency),
#                    interval_mean=median(interval_mean),
#                    delay_last=median(delay_last),
#                    delay_avg=median(delay_avg),
#                    delay_trend=median(delay_trend),
#                    cost_total=median(cost_total),
#                    cost_trend=median(cost_trend),
#                    cost_avg=median(cost_avg),
#                    part_avg=median(part_avg),
#                    part_last=median(part_last),
#                    count = n())
# 
# write.csv(profile,paste("../4. Data/Processed data/",dealer.name,"_profile 2 segment.csv",sep = ""),row.names = FALSE)
# 
# # 3 segments
# profile <- cust.target %>%
#   select(segment,
#          age,
#          ownership,
#          visit_year,
#          warranty,
#          warranty_labor,
#          duration_avg,
#          duration_last,
#          milage,
#          mileage_day,
#          visit_avg,
#          visit_last,
#          visit_trend,
#          recency,
#          interval_mean,
#          delay_last,
#          delay_avg,
#          delay_trend,
#          cost_total,
#          cost_trend,
#          cost_avg,
#          cost_visit,
#          part_avg,
#          part_last) %>%
#   group_by(segment) %>%
#   dplyr::summarise(age = median(age,na.rm = TRUE), 
#                    ownership = median(ownership),
#                    visit_year=median(visit_year),
#                    warranty=sum(warranty),
#                    warranty_labor=sum(warranty_labor),
#                    duration_avg=median(duration_avg),
#                    duration_last=median(duration_last),
#                    milage=median(milage),
#                    mileage_day=median(mileage_day),
#                    visit_avg=median(visit_avg),
#                    visit_last=median(visit_last),
#                    visit_trend=median(visit_trend),
#                    recency=median(recency),
#                    interval_mean=median(interval_mean),
#                    delay_last=median(delay_last),
#                    delay_avg=median(delay_avg),
#                    delay_trend=median(delay_trend),
#                    cost_total=median(cost_total),
#                    cost_trend=median(cost_trend),
#                    cost_avg=median(cost_avg),
#                    part_avg=median(part_avg),
#                    part_last=median(part_last),
#                    count = n())
# 
# write.csv(profile,paste("../4. Data/Processed data/",dealer.name,"_profile 3 segment.csv",sep = ""),row.names = FALSE)

# 10 segment
profile <- cust.target %>%
  select(segment2,
         age,
         ownership,
         visit_year,
         warranty,
         warranty_labor,
         last_PM,
         duration_avg,
         duration_last,
         wash,
         wait,
         visit_disc_p,
         milage,
         mileage_day,
         visit_avg,
         visit_last,
         visit_trend,
         visit,
         recency,
         interval_mean,
         delay_last,
         delay_avg,
         delay_trend,
         cost_total,
         cost_trend,
         cost_avg,
         cost_visit,
         part_avg,
         part_last) %>%
  group_by(segment2) %>%
  dplyr::summarise(age = median(age,na.rm = TRUE), 
                   ownership = median(ownership),
                   visit_year=median(visit_year),
                   warranty=sum(warranty),
                   warranty_labor=sum(warranty_labor),
                   last_PM=median(last_PM),
                   duration_avg=median(duration_avg),
                   duration_last=median(duration_last),
                   wash=median(wash),
                   wait=median(wait),
                   visit_disc_p=median(visit_disc_p),
                   milage=median(milage),
                   mileage_day=median(mileage_day),
                   visit_avg=median(visit_avg),
                   visit_last=median(visit_last),
                   visit_trend=median(visit_trend),
                   visit=median(visit),
                   recency=median(recency),
                   interval_mean=median(interval_mean),
                   delay_last=median(delay_last),
                   delay_avg=median(delay_avg),
                   delay_trend=median(delay_trend),
                   cost_total=median(cost_total),
                   cost_trend=median(cost_trend),
                   cost_avg=median(cost_avg),
                   part_avg=median(part_avg),
                   part_last=median(part_last),
                   count = n())

write.csv(profile,paste("../4. Data/Processed data/",dealer.name,"_profile 10 segment.csv",sep = ""),row.names = FALSE)

#############################################################
## model distribution
model_dist <- as.data.frame.matrix(table(cust.target$segment,cust.target$model))
write.csv(model_dist,paste("../4. Data/Processed data/",dealer.name,"_model distribution.csv",sep = ""),row.names = FALSE)
