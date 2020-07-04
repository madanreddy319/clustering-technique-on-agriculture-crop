######Prepare and load the dataset
yield.data <- read.csv("~/Desktop/artefact/District-wise, season-wise crop production statistics from 1997.csv")
data <- na.omit(yield.data)
dataset <- data.matrix(data)
dataset

smple <- dataset[sample(nrow(dataset)),]
sample_short <- smple[, c(6,7)]
sample_matrix <- data.matrix(sample_short)

### removing the null values from the dataset with the below function
sample <- na.omit(sample_matrix)
sample <- data.frame(sample)

#### Now, let’s determine the number of clusters.

opt_gmm = Optimal_Clusters_GMM(dataset, max_clusters = 5, criterion = "BIC", dist_mode = "maha_dist", seed_mode = "random_subset", km_iter = 10, em_iter = 10, var_floor = 1e-10,   plot_data = T)

####Installation
### You can install the released version of mclust from CRAN using:
  
install.packages("mclust")
library(mclust)
library(caret)

# Fit Gaussian Mixture Model
k <- 3 # no. of clusters
model <- Mclust(sample, G=3)
model$classification
###Assuming that true labels are available, then one could use the external validation methods(rand index, adjusted rand index, Jaccard index, Fowlkes Mallows index) to validate the output clusters.
res = external_validation(model$classification,data$Production, 
                          
                          method = "adjusted_rand_index", summary_stats = T)

res

####we can check the accuracy of the classification clusters in the above figure accuracy : 0.5685(56.85%)

g1 <- data[model$classification == 1, c(1,3,4,6,7)]  #cols  = state name, area, Production
g2 <- data[model$classification == 2, c(1,3,4,6,7)] 
g3 <- data[model$classification == 3, c(1,3,4,6,7)] 
summary(g1)
summary(g2)
summary(g3)

g1.prod.year <- aggregate(g1$Production ~ g1$State_Name+g1$Crop_Year, FUN = sum)
names(g1.prod.year) <- c("State_Name","Year","Production")

###install.packages("plotly")
library(plotly)

g<-ggplot(data = g1.prod.year) + aes( x=Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop Year") + ylab("Production") + ggtitle("Total Production by cluster g1")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")


### to check the which states are in cluster 2

summary(g2)

#######

g2.prod.year <- aggregate(g2$Production ~ g2$State_Name+g2$Crop_Year, FUN = sum)
names(g2.prod.year) <- c("State_Name","Year","Production")

g<-ggplot(data = g2.prod.year) + aes( x=Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop Year") + ylab("Production") + ggtitle("Total Production by cluster g2")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")


### to check the which states are in cluster 3

summary(g3)

########

g3.prod.year <- aggregate(g3$Production ~ g3$State_Name+g3$Crop_Year, FUN = sum)
names(g3.prod.year) <- c("State_Name","Year","Production")

g<-ggplot(data = g3.prod.year) + aes( x=Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop_Year") + ylab("Production") + ggtitle("Total Production by cluster g3")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")

# who is in the top and bottom groups?
top <- which.max(c(max(g1$Production),max(g2$Production), max(g3$Production)))

bottom <- which.min(c(max(g1$Production),max(g2$Production), max(g3$Production)))

print(paste("production Group is g", top, sep=""))
print(paste("Least production Group is g", bottom, sep=""))

library(ClusterR)

# predict centroids, covariance matrix and weights


library(ClusterR)
data.matrix <- data.matrix(data)
train <- scale(data.matrix[,-7])
gmm <- GMM(train,3)
gmm
test <- scale(data.matrix[,c(7)])
scale <-GMM(test,3)

pr = predict_GMM(train, gmm$centroids, gmm$covariance_matrices, gmm$weights)    

pr$cluster_labels 

###Assuming that true labels are available, then one could use the external validation methods(rand index, adjusted rand index, Jaccard index, Fowlkes Mallows index) to validate the output clusters.

res = external_validation(pr$cluster_labels,data$Production, 
                          
                          method = "adjusted_rand_index", summary_stats = T)

res
### accuracy OR rand-index         : 0.5952 

pred <- as.factor(pr$cluster_labels+1)

### prediction

top.pred <- print(paste("Predict which states is in the top crop production group, Group", top, sep=""), row.names = FALSE)
pred.top <- print(data[pred == top, ], row.names = FALSE)
summary(pred.top)


library(ggplot2)
g<-ggplot(data = pred.top) + aes( x=pred.top$Crop_Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop_Year") + ylab("Production") + ggtitle("predicted top crop production states")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")

####Bottom Group

print(paste("Predict which states is in the least crop production group, Group", bottom, sep=""), row.names = FALSE)

pred.bottom <- print(data[pred == bottom, ], row.names = FALSE)
summary(pred.bottom)


g<-ggplot(data = pred.bottom) + aes( x=pred.bottom$Crop_Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop_Year") + ylab("Production") + ggtitle("predicted low crop production states")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")


