rm(list = ls())
# load library
library(caret)
library(AppliedPredictiveModeling)
library(ggplot2)
library(plyr)

# get work directory
wd=getwd();

# load function

#############################################################
## set parameters
dealer.name <- "11155"
branch.name <- "HO"
# for active customer
cutoff <- as.Date("2017-04-01")

#############################################################
## load in data
cust.target <- read.csv(paste("../4. Data/Processed data/",dealer.name,"_target customer data.csv",sep = ""))
# make target factor
cust.target$retain <- as.factor(ifelse(cust.target$retain==0,"Churned","Retained"))

#############################################################
## data explore single variant



#############################################################
## data explore against target
# boxplot
# ownership
ggplot(cust.target, aes(x=retain, y=ownership, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$ownership, c(0.1, 0.9)))

# age
ggplot(cust.target, aes(x=retain, y=age, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$age, c(0.1, 0.9)))

# milage
ggplot(cust.target, aes(x=retain, y=milage, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$milage, c(0.1, 0.9)))

# visit
ggplot(cust.target, aes(x=retain, y=visit, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$visit, c(0.1, 0.9)))

# visit_avg average per year
p_meds <- ddply(cust.target, .(retain), summarise, med = median(visit_avg))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=retain, y=visit_avg, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$visit_avg, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = retain, y = med, label = med), 
            size = 4)

# interval
p_meds <- ddply(cust.target, .(retain), summarise, med = median(interval_mean))
p_meds$med <- round(p_meds$med,2)
ggplot(cust.target, aes(x=retain, y=interval_mean, fill=retain)) + 
  geom_boxplot(alpha=0.3,outlier.shape = NA) +
  theme(legend.position="none")+
  scale_y_continuous(limits = quantile(cust.target$interval_mean, c(0.1, 0.9)))+
  geom_text(data = p_meds, aes(x = retain, y = med, label = med), 
            size = 4)