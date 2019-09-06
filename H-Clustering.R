View(University_Clustering)
attach(University_Clustering)

# 1. Stadardize the data
# Remove the first and second columns for analysis

mydata <- University_Clustering[, c(3:8)]
mydata
View(mydata)

# Normalize the data

normalize_data<-scale(mydata)
normalize_data
View(normalize_data)

# 2. Calculate the Distance between variables using Euclidean method

distance <- dist(normalize_data , method = "euclidean")
distance
summary(distance)

# 3. Grouping the variables using single linkage method or complete linkage method

fit <- hclust(distance,method =  "complete")
fit

# check the clustering using Dendogram

plot(fit)

# make all the variable in one line

plot(fit,hang=-1)

# 4. looking into dendogram decide how many cluster we need 
# we are diving the dendogram into 3 clustering by taking k=3


groups<-cutree(fit,k=3)
groups

# labelling the groups with red colour

rect.hclust(fit,k=3,border="red")

#to check the syntax for cutting tree

library(cluster)
clusplot(normalize_data,
         groups,
         lines = 0,labels = 2,color = TRUE,  shade = TRUE,
         main = paste('Clusters of University'))

# in data set label the clusters or grouping in dataset

membership<-as.matrix(groups)
View(membership)

final1<-data.frame(University_Clustering,membership)
View(final1)

# save the file on desktop

write.xlsx(final1,file="jan1st.xlsx")
getwd()
