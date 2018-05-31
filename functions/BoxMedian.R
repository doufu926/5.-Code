# function to plot boxplot
library(dplyr)
BoxMedian <- function(data, target, attribute){
  p_meds <- ddply(data, .(target), summarise, med = median(attribute))
  p_meds$med <- round(p_meds$med,2)
  ggplot(data, aes(x=target, y=attribute, fill=target)) + 
    geom_boxplot(alpha=0.3,outlier.shape = NA) +
    theme(legend.position="none")+
    scale_y_continuous(limits = quantile(data$attribute, c(0.1, 0.9)))+
    geom_text(data = p_meds, aes(x = target, y = med, label = med), 
              size = 4)
}