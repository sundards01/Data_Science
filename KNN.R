# drop the id feature
View(wbcd)
wbcd1<-wbcd[,-1]
View(wbcd1)

# table of diagnosis -> calculating number of entries with B and M 

summary(wbcd1$diagnosis)

# recode diagnosis as a factor -> giving the full name to labels 

wbcd1$diagnosis <- factor(wbcd1$diagnosis, levels = c("B", "M"),labels = c("Benign", "Malignant"))
View(wbcd1)

# table or proportions with more informative labels  -> calcualting the probablity

round(prop.table(table(wbcd1$diagnosis)) * 100, digits = 1)

# summary of dataset

summary(wbcd1)

# summarize three numeric features

summary(wbcd1[c("radius_mean", "area_mean", "smoothness_mean")])


str(wbcd)

# create normalization function

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# test normalization function - result should be identical
#normalize(c(1, 2, 3, 4, 5))
#normalize(c(10, 20, 30, 40, 50))

# normalize the wbcd data -> applying the normalize function which is created in line 32

wbcd1_n <- as.data.frame(lapply(wbcd1[2:31], normalize))

# confirm that normalization worked
View(wbcd1_n)
summary(wbcd1_n)

# create training and test data

wbcd1_train <- wbcd1_n[1:469, ]
View(wbcd1_train)

wbcd1_test <- wbcd1_n[470:569, ]
View(wbcd1_test)

# create labels for training and test data

wbcd_train_labels <- wbcd1[1:469, 1]
View(wbcd_train_labels)
wbcd_test_labels <- wbcd1[470:569, 1]
View(wbcd_test_labels)

#---- Training a model on the data ----

# load the "class" library
library(class)
wbcd_train_labels <- wbcd_train_labels[["diagnosis"]]
View(wbcd_train_labels)
wbcd_test_labels <- wbcd_test_labels[["diagnosis"]]

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test,cl = wbcd_train_labels, k=23)
View(wbcd_test_pred)

##--------Evaluating model performance ----

# load the "gmodels" library
install.packages("gmodels")
library(gmodels)

# Create the cross tabulation of predicted vs. actual

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)

## Improving model performance ----

# re-classify test cases
wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test,cl = wbcd_train_labels, k=2)

# Create the cross tabulation of predicted vs. actual
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,prop.chisq=FALSE)


wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd1_train, test = wbcd1_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)


