# View of my dataset

View(university)

# remove first and second columns from the data set as these are not required for EDA

mydata <- university[,3:8]
View(mydata)

# Normalize the data

scaling <- scale(mydata)
View(scaling)

# Decide the k values and group them into clusters as per k value

kmeanscluster <-kmeans(scaling,4)
str(kmeanscluster)

# now we have to group by calculating centrioid and finding the distance between centroids and variable and group the
#variable as per least distance

install.packages("animation")
library(animation)
km <- kmeans.ani(scaling, 4)
km$centers

# Determine number of clusters
wss <- (nrow(scaling)-1)*sum(apply(scaling,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(scaling, centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# labelling the clustering number into dataset
final2<- data.frame(mydata, km$cluster) # append cluster membership
final2

# making membership varaible as first column.

final3 <- final2[,c(ncol(final2),1:(ncol(final2)-1))]
final3

# Load the package for writing the data into xlsx file format 
library(xlsx)

# save the file on desktop

write.xlsx(final3,file="final1.xlsx")

# To check directory 
getwd()


# This is K -Means clustering from Quick-R

# Prepare Data

mydata <- na.omit(university) # listwise deletion of missing
View(university)
mydata5 <- scale(university[,3:8]) # standardize variables
View(mydata5)

# Determine number of clusters

wss <- (nrow(mydata5)-1)*sum(apply(mydata5,2,var))

for (i in 2:15) wss[i] <- sum(kmeans(mydata5, centers=i)$withinss)

plot(1:15, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")

# K-Means Cluster Analysis

fit <- kmeans(mydata5, 5) # 5 cluster solution
fit
# get cluster means 

aggregate(mydata5,by=list(fit$cluster),FUN=mean)

# append cluster assignment

mydata6 <- data.frame(mydata5, fit$cluster)
View(mydata6)

# Ward Hierarchical Clustering

d <- dist(mydata6, method = "euclidean") # distance matrix
fit <- hclust(d, method="ward.D2") 
plot(fit) # display dendogram
groups <- cutree(fit, k=5) # cut tree into 5 clusters

# draw dendogram with red borders around the 5 clusters 
rect.hclust(fit, k=5, border="red")

# Ward Hierarchical Clustering with Bootstrapped p values

install.packages("pvclust")
library(pvclust)
fit <- pvclust(mydata6, method.hclust="ward.D2",method.dist="euclidean")
plot(fit) # dendogram with p values

# add rectangles around groups highly supported by the data

pvrect(fit, alpha=.95)

# Model Based Clustering

install.packages("mclust")
library(mclust)
fit <- Mclust(mydata6)

plot(fit) # plot results 
summary(fit)
# display the best model

# K-Means Clustering with 5 clusters
fit <- kmeans(mydata5, 5)

# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster) 
clusplot(mydata5, fit$cluster, color=TRUE, shade=TRUE,labels=2, lines=0)

# Centroid Plot against 1st 2 discriminant functions
library(fpc)
plotcluster(mydata5, fit$cluster)

# comparing 2 cluster solutions
library(fpc)
cluster.stats(d, fit$cluster,fit$cluster)