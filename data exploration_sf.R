rm(list = ls())
# load library
library(caret)
library(AppliedPredictiveModeling)
library(ggplot2)
library(dplyr)


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
cust.target <- read.csv(paste("../4. Data/Processed data/","test_data_results_",dealer.name,".csv",sep = ""))
# make target factor
# cust.target$CHURN <- as.factor(ifelse(cust.target$CHURN==1,"Churned","Retained"))

#############################################################
## segment analysis
# by decile
segment <- cust.target %>%
  select(Decile,CHURN,Predict) %>%
  group_by(Decile) %>%
  dplyr::summarise(actual = sum(CHURN), predict = sum(Predict),number = n())
segment$acccuracy=segment$actual/segment$number
segment <- subset(segment,segment$Decile!=0)

# by risk
cust.target$risk.bin <- cut(cust.target$Risk,breaks = seq(0,1,0.1),labels = seq(10,1))
cust.target$segment <- cut(cust.target$Risk,breaks = c(0,0.3,0.7,1),labels = c("low","mid","high"))
segment2 <- cust.target %>%
  select(segment,CHURN,Predict) %>%
  group_by(segment) %>%
  dplyr::summarise(actual = sum(CHURN), predict = sum(Predict),number = n())
segment2$acccuracy=segment2$actual/segment2$number

#############################################################
## segment profile - aggregate
segment3 <- cust.target %>%
  select(risk.bin,CUST_DOB,car_age,avg_mil,next_in_warranty,median_interval,total_cost_avg_last_24_mths,total_visit_last_12_mths) %>%
  group_by(risk.bin) %>%
  dplyr::summarise(age = median(CUST_DOB), ownership = median(car_age),mileage=median(avg_mil),warranty=sum(next_in_warranty),interval=median(median_interval),cost=median(total_cost_avg_last_24_mths),visit=median(total_visit_last_12_mths))

segment4 <- cust.target %>%
  select(segment,CUST_DOB,car_age,avg_mil,MILEAGE_OUT,next_in_warranty,median_interval,total_cost_avg_last_12_mths,total_visit_last_12_mths) %>%
  group_by(segment) %>%
  dplyr::summarise(age = median(CUST_DOB), ownership = median(car_age),mileage=median(MILEAGE_OUT),mileage_day=median(avg_mil),warranty=sum(next_in_warranty),interval=median(median_interval),cost=median(total_cost_avg_last_12_mths),visit=median(total_visit_last_12_mths))
#############################################################
## segment profile - distribution
# mileage distribution
p_meds <- ddply(cust.target, .(segment), summarise, med = median(MILEAGE_OUT))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=segment, y=MILEAGE_OUT, fill=segment)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$MILEAGE_OUT, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = segment, y = med, label = med), 
            size = 4,vjust = 1)

a=boxplot(MILEAGE_OUT~segment,data = cust.target)
a$stats


p_meds <- ddply(cust.target, .(CHURN), summarise, med = median(MILEAGE_OUT))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=CHURN, y=MILEAGE_OUT, fill=CHURN)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$MILEAGE_OUT, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = CHURN, y = med, label = med), 
            size = 4,vjust = 1)

a=boxplot(MILEAGE_OUT~CHURN,data = cust.target)
a$stats

# mileage/day distribution
p_meds <- ddply(cust.target, .(segment), summarise, med = median(avg_mil))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=segment, y=avg_mil, fill=segment)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$avg_mil, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = segment, y = med, label = med), 
            size = 4,vjust = 0.5)

a=boxplot(avg_mil~segment,data = cust.target)
a$stats


p_meds <- ddply(cust.target, .(CHURN), summarise, med = median(avg_mil))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=CHURN, y=avg_mil, fill=CHURN)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$avg_mil, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = CHURN, y = med, label = med), 
            size = 4,vjust = 1)

a=boxplot(avg_mil~CHURN,data = cust.target)
a$stats

# ownership distribution
p_meds <- ddply(cust.target, .(segment), summarise, med = median(car_age))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=segment, y=car_age, fill=segment)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$car_age, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = segment, y = med, label = med), 
            size = 4,vjust = 0.5)

a=boxplot(car_age~segment,data = cust.target)
a$stats


p_meds <- ddply(cust.target, .(CHURN), summarise, med = median(car_age))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=CHURN, y=car_age, fill=CHURN)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$car_age, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = CHURN, y = med, label = med), 
            size = 4,vjust = 1)

a=boxplot(car_age~CHURN,data = cust.target)
a$stats

# interval distribution
p_meds <- ddply(cust.target, .(segment), summarise, med = median(median_interval))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=segment, y=median_interval, fill=segment)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$median_interval, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = segment, y = med, label = med), 
            size = 4,vjust = 0.5)

a=boxplot(median_interval~segment,data = cust.target)
a$stats


p_meds <- ddply(cust.target, .(CHURN), summarise, med = median(median_interval))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=CHURN, y=median_interval, fill=CHURN)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$median_interval, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = CHURN, y = med, label = med), 
            size = 4,vjust = 1)

a=boxplot(median_interval~CHURN,data = cust.target)
a$stats