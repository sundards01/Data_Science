View(universities) # view of the dataset
mydata<- universities[-1] # remove the first column whihc is not required for the analysis
View(mydata)

pcaobj <-princomp(mydata , cor = TRUE , scores = TRUE , covmat = NULL ) # Calculate Principal component
summary(pcaobj)

plot(pcaobj) # plot of the data
biplot(pcaobj)

pcaobj$scores # it is will dispaly all the columns
pcaobj$scores[, 1:4] # it will display only 4 columns whica re required for analysis and it will give 97% of accuracy of data

# Considering top 4 principal component scores and binding them with mydata

mydata<-cbind(mydata,pcaobj$scores[,1:4]) #  cbind used to bind the data in column wise
View(mydata) 

# preparing data for clustering (considering only pca scores as they represent the entire data)
clus_data<-mydata[,7:10]

# Normalizing the data 

norm_clus<-scale(clus_data) # Scale function is used to normalize data
dist1<-dist(norm_clus,method = "euclidean") # method for finding the distance


# Clustering the data using hclust function --> Hierarchical

fit1<-hclust(dist1,method="complete") # method here is complete linkage
plot(fit1) # Displaying Dendrogram

groups<-cutree(fit1,3) # Cutting the dendrogram for 3 clusters
membership_1<-as.matrix(groups) # cluster numbering 

View(membership_1)
final1<-cbind(membership_1,mydata) # binding column wise with orginal data
View(final1)

View(aggregate(final1[,-c(2,9:11)],by=list(membership_1),FUN=mean))  # drawn from the aggregate of the universities data on membership_1
