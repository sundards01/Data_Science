library(caret)
dataset<-wbcd # rename the wbcd to dataset
View(dataset)
# create a list of 80% of the rows in the original dataset we can use for training

validation_index <- createDataPartition(dataset$diagnosis, p=0.80, list=FALSE)

# use the remaining 80% of data to training and testing the models

dataset1 <- dataset[validation_index,]
View(dataset1)

# select 20% of the data for validation

validation <- dataset1[-validation_index,]
View(validation)

# dimensions of dataset
dim(dataset1)

# list types for each attribute
sapply(dataset1, class)

# take a peek at the first 5 rows of the data
head(dataset1)

# list the levels for the class
levels(dataset1$diagnosis)

# summarize the class distribution
percentage <- prop.table(table(dataset1$diagnosis)) * 100
cbind(freq=table(dataset1$diagnosis), percentage=percentage)

# summarize attribute distributions
summary(dataset1)

# split input and output
x <- dataset1[,3:32]
x
y <- dataset[,2]
y

# boxplot for each attribute on one image
par(mfrow=c(3,7))
for(i in 3:7) {
  boxplot(x[,i], main=names(dataset1)[i])
}

# barplot for class breakdown
plot(y)

# scatterplot matrix
featurePlot(x=x, y=y, plot="ellipse")

# box and whisker plots for each attribute
featurePlot(x=x, y=y, plot="box")

# density plots for each attribute by class value
scales <- list(x=list(relation="free"), y=list(relation="free"))
featurePlot(x=x, y=y, plot="density", scales=scales)

# Run algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

# a) linear algorithms
set.seed(7)
fit.lda <- train(diagnosis~., data=dataset1, method="lda", metric=metric, trControl=control)
print(fit.lda)
# b) nonlinear algorithms
# CART
set.seed(7)
fit.cart <- train(diagnosis~., data=dataset1, method="rpart", metric=metric, trControl=control)
print(fit.cart)
# kNN
set.seed(7)
fit.knn <- train(diagnosis~., data=dataset1, method="knn", metric=metric, trControl=control)
print(fit.knn)
# c) advanced algorithms
# SVM
set.seed(7)
fit.svm <- train(diagnosis~., data=dataset1, method="svmRadial", metric=metric, trControl=control)
print(fit.svm)
# Random Forest
set.seed(7)
fit.rf <- train(diagnosis~., data=dataset1, method="rf", metric=metric, trControl=control)
print(fit.rf)

# summarize accuracy of models
results <- resamples(list(lda=fit.lda, cart=fit.cart, knn=fit.knn, svm=fit.svm, rf=fit.rf))
summary(results)

# compare accuracy of models
dotplot(results)

# estimate skill of LDA on the validation dataset
predictions <- predict(fit.svm, validation)
confusionMatrix(predictions, validation$diagnosis)

#----------------------------------------------------

# with PCA

View(dataset)
dataset2 <- dataset[,-1 :-2] # remove first and second columns which are not required for analysis
View(dataset2)
pcaObj<-princomp(dataset2, cor = TRUE, scores = TRUE, covmat = NULL) # Apply PCA
summary(pcaObj)
pcaObj$scores[,1:12] # only 12 columns considered
dataset2<-cbind(dataset2,pcaObj$scores[,1:12]) # Add these 12 columns in to excel
View(dataset2)
clus_data<-dataset2[,31:42] # Only those 12 columns will take for analysis
View(clus_data)
scale(clus_data) # normalize the data

