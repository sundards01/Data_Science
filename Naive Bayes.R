# we load mlbench, fetch the dataset 

library(mlbench)

# uploading housevotes dataset 

data()
data("HouseVotes84")
View(HouseVotes84)
mydata<-na.omit(HouseVotes84) # remove missing data vlaues
View(mydata)
help("HouseVotes84")

# Exploratory Data Analysis

summary(HouseVotes84)
summary(mydata)

barplot(table(as.factor(HouseVotes84[,1]),as.factor(HouseVotes84[,2])),legend=c("democrat","republic"))
plot(as.factor(HouseVotes84[HouseVotes84$Class=="republican",2]))
plot(as.factor(HouseVotes84[HouseVotes84$Class=="democrat",2]))
str(HouseVotes84)
str(mydata)


#train_x<-HouseVotes84[,c()]
# imputing missing values 
# We do this by randomly assigning values ( y or n) to NAs, based on the proportion of members 
#of a party who have voted y or n

#na.omit(train)
#newdata <- na.omit(is.null(NA))
#View(newdata)

set.seed(3)
train<-order(runif(290))
test<--train
View(train)
View(test)

# Divide the data into Training and Testing Data 

training<-HouseVotes84[train,]
View(training)
testing<-HouseVotes84[test,]
View(testing)

# Building the naive Baye model

library(e1071)

model<-naiveBayes(training$Class~.,data=training)
model

# Predicting the model on Test data

pred<-predict(model,newdata = testing[,-1])
mean(pred==testing[,1])
