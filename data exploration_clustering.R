rm(list = ls())
# load library
library(ggplot2)
library(dplyr)
library(kohonen)

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
cust.target$interval_max <- cust.target2$max_interval[match(cust.target$VIN_NO,cust.target2$VIN_NO)]
# define segment
cust.target$segment <- cut(cust.target$risk,breaks = c(0,0.3,0.7,1),labels = c("Low","Mid","High"))
cust.target$segment2 <- cut(cust.target$risk,breaks = seq(0,1,0.1),labels = seq(1,10))
cust.target$segment3 <- with(cust.target, cut(risk, 
                                              breaks=quantile(risk, probs=seq(0,1, by=0.1), na.rm=TRUE), 
                                              include.lowest=TRUE,labels = seq(1,10)))
# select customer in one segment
cust.target <- subset(cust.target,cust.target$segment=="High")
#############################################################
## select variables for SOM training
cust.target.train <- cust.target[,c("ownership",
                                      "age",
                                      "mileage_day",
                                      "visit_year",
                                      "visit_avg",
                                      "recency",
                                      "interval_mean",
                                      "interval_max",
                                      "cost_visit",
                                      "delay_avg",
                                      "part_avg",
                                      "visit_disc_p",
                                      "last_PM")]

# cust.target.train <- cust.target[,c("ownership",
#                                     "mileage_day",
#                                     "visit_year",
#                                     "last_PM",
#                                     "risk")]

#############################################################
## train model
# change to matrix and scale and center each attribute
data_train_matrix <- as.matrix(scale(cust.target.train))

# som grid
som_grid <- somgrid(xdim = 10, ydim=10, topo="hexagonal")

# train
som_model <- som(data_train_matrix, 
                 grid=som_grid, 
                 rlen=1000, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE, maxNA.fraction=.5)

# som_model <- xyf(data_train_matrix, 
#                  cust.target$segment,
#                  grid=som_grid, 
#                  rlen=1000, 
#                  alpha=c(0.05,0.01), 
#                  keep.data = TRUE, maxNA.fraction=.5)

# plot x and y 
par(mfrow = c(1,2))
plot(som_model, type="codes", main = c("Codes X", "Codes Y"))
#############################################################
## visualization - overall
source('coolBlueHotRed.R')
# Plot of the training progress - how the node distances have stabilised over time
plot(som_model, type="changes")

#counts within nodes
plot(som_model, type = "counts", main="Node Counts", palette.name=coolBlueHotRed)

#map quality
plot(som_model, type = "quality", main="Node Quality/Distance", palette.name=coolBlueHotRed)

#neighbour distances
plot(som_model, type="dist.neighbours", main = "SOM neighbour distances", palette.name=grey.colors)

#code spread
plot(som_model, type = "codes")

#############################################################
## visualization - heatmap
#plot a variable from the original data set (will be uncapped etc.)
# This function produces a menu for multiple heatmaps.
source('plotHeatMap.R')
plotHeatMap(som_model, cust.target, variable=37)

#############################################################
## clustering
# kmeans can be used as a "rough" indicator of the ideal number of clusters
mydata <- som_model$codes[[1]]
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
par(mar=c(5.1,4.1,4.1,2.1))
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares", main="Within cluster sum of squares (WCSS)")

# Form clusters on grid
## use hierarchical clustering to cluster the codebook vectors
som_cluster <- cutree(hclust(object.distances(som_model, "codes")), 3)
add.cluster.boundaries(som_model, som_cluster)
