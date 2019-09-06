# Using Random Forest
install.packages("randomForest")
library(randomForest)
data()
data(iris)
View(iris)

# Splitting data into training and testing. As the species are in order 
# splitting the data based on species 

iris_setosa<-iris[iris$Species=="setosa",] # divide the data based on setosa
View(iris_setosa)
iris_versicolor <- iris[iris$Species=="versicolor",] # divide the data based on versicolor
View(iris_versicolor)
iris_virginica <- iris[iris$Species=="virginica",] # divide the data based on virginica
View(iris_virginica)

iris_train <- rbind(iris_setosa[1:25,],iris_versicolor[1:25,],iris_virginica[1:25,])
View(iris_train)
iris_test <- rbind(iris_setosa[26:50,],iris_versicolor[26:50,],iris_virginica[26:50,])
View(iris_test)

# Building a random forest model on training data 

model1 <- randomForest(Species~.,data=iris_train)
model1

# Output show 
# Number of Trees is 500 , Number of variables tried at each spilit is 2
# Error Rate is 4 % 


# Fine tuning parameters of Random Forest model
model2 <- randomForest(Species~.,data=iris_train, ntree = 100, importance = TRUE)
model2

#To check Importance variables 

importance(model2)
varImpPlot(model2)
plot(round(importance(model2)))
?randomeforest  # Help for random froest
table(predict(model1),iris_train$Species)

# Predicting test data 

pred_test <- predict(model1,newdata=iris_test)
table(pred_test,iris_test$Species)

#Checking the accuracy by creating confusion matrix 

CM <- table(pred_test,iris_test$Species)
CM
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

# predicting for Model2

pred_test <- predict(model2,newdata=iris_test)
table(pred_test,iris_test$Species)

#Checking the accuracy by creating confusion matrix 

CM <- table(pred_test,iris_test$Species)
CM
accuracy <- (sum(diag(CM)))/sum(CM)
accuracy

