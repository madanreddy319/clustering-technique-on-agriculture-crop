install.packages("dplyr")
library(dplyr)
install.packages("data.table")
library("data.table")

install.packages("bnlearn")
library(bnlearn)                       # Load the package in R
install.packages("Rcpp") 
install.packages("forecast", dependencies = TRUE) 
library(forecast)

######Prepare and load the dataset
yield.data <- read.csv("~/Desktop/thesis.project/District-wise, season-wise crop production statistics from 1997.csv")
data  <- na.omit(yield.data)

dataset <- data.matrix(data)

smple <- dataset[sample(nrow(dataset)),]
sample_short <- smple[, c(6,7)]
sample_matrix <- data.matrix(sample_short)

### removing the null values from the dataset with the below function
sample <- na.omit(sample_matrix)


#####Now, let’s determine the number of clusters.

wss <- (nrow(sample)-1)*sum(apply(sample,2,var))

for (i in 1:5) wss[i]<-sum(kmeans(sample,centers=i)$withinss)

plot(wss, type="b", xlab="Number of Clusters", ylab="Within Sum of Squares")

###### It gives the elbow plot as follows.


#### Now check the kmeans with 3 clusters with the sample as data 

cl <- kmeans(sample,3,nstart=25)

cl

## Here, total_SS is the sum of squared distances of each data point to the global sample mean. 
#whereas between_SS is the sum of squared distances of the cluster centroids to the global mean. 
# Here, 92.5 % is a measure of the total variance in the data set. 
# The goal of k-means is to maximize the between-group dispersion(between_SS). 
#So, higher the percentage value, better is the model.


### You can plot the graph and cluster centroid using the following command.

plot(sample, col =(cl$cluster) , main="k-means result with 3 clusters", pch=1, cex=1, las=1)

points(cl$centers, col = "black", pch = 17, cex = 2)

### For a more in-depth look at the clusters, 
### we can examine the coordinates of the cluster centroids using the cl$centers component, 
### which is as follows for Area, Production and Crop_Year.

cl


###########  to know that which states has the largest crop and lower crop in the 3 clusters
ind <- sample(nrow(yield.data),c(7))
# add TRUE / FALSE index to data

yield.data[["crop"]] <- TRUE
yield.data[["crop"]][ind] <- FALSE
crop <- yield.data[yield.data[["crop"]]==TRUE, ]
crop.test <- yield.data[yield.data[["crop"]]==FALSE, ]
crop.net <- data.matrix(crop)

crop.na <- na.omit(crop)
library("flexclust")
# create K-Means cluster of 3 groups based on cols 

km_net = kcca(crop.na[ , c(7)], k=3, kccaFamily("kmeans"))
km_net

g1 <- crop.na[clusters(km_net) == 1, c(1,3,4,6,7)]  #cols  = state name, area, Production
g2 <- crop.na[clusters(km_net) == 2, c(1,3,4,6,7)] 
g3 <- crop.na[clusters(km_net) == 3, c(1,3,4,6,7)] 

### to check the which states are in cluster 1
summary(g1) 

########

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

g<-ggplot(data = g3.prod.year) + aes( x=Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("State_Name") + ylab("Production") + ggtitle("Total Production by cluster g3")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")

# who is in the top and bottom groups?
top <- which.max(c(max(g1$Production),max(g2$Production), max(g3$Production)))

bottom <- which.min(c(max(g1$Production),max(g2$Production), max(g3$Production)))

print(paste("production Group is g", top, sep=""))
print(paste("Least production Group is g", bottom, sep=""))


### prediction

actual <- scale(dataset[,-7])
training<-kmeans(actual, 3, iter.max = 10, nstart = 1)

#install.packages("clue")
library(clue)
# predict classes for testing data
cl_predict <- cl_predict(training)
cl_predict
summary(cl_predict)
summary(cl$cluster)

top.pred <- print(paste("Predict which states is in the top crop production group, Group", top, sep=""), row.names = FALSE)
pred.top <- print(data[cl_predict == top, ], row.names = FALSE)
summary(pred.top)


library(ggplot2)
g<-ggplot(data = pred.top) + aes( x=pred.top$Crop_Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop_Year") + ylab("Production") + ggtitle("predicted top crop production states")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")

####Bottom Group

bottom.pred <- print(paste("Predict which states is in the least crop production group, Group", bottom, sep=""), row.names = FALSE)

pred.bottom <- print(data[cl_predict == bottom, ], row.names = FALSE)
summary(pred.bottom)


g<-ggplot(data = pred.bottom) + aes( x=pred.bottom$Crop_Year, y = Production, fill=State_Name) + geom_bar(stat = "identity") + xlab("Crop_Year") + ylab("Production") + ggtitle("predicted low crop production states")
g + theme(legend.position="bottom", legend.box = "horizontal") + labs(subtitle="State Names are at Bottom")


# cross tabulation

xtab <- table(cl_predict,data$Production)
xtab
####As seen from the table we have three classes and only three clusters which means the data must not be perfect for this analysis and an not all classes would be classified correctly.
####We can use the adjusted rank index(randIndex()) from the flexclust package to measure the agreement between the three partitions.
#####The range of the index is -1 to 1 where -1 has no agreement and 1 is perfect agreement.
###install.packages("flexclust")
library(flexclust)
library(ClusterR)

res = external_validation(cl_predict,data$Production, 
                          
                          method = "adjusted_rand_index", summary_stats = T)

res

####accuracy OR rand-index         : 0.66

####0.0005843772

