# 1. Exploratory Data Analysis

# First Business Moment
View(Cars)
attach(Cars)

mean(Cars$MPG) # Calculate Mean

median(Cars$MPG) # Calculate Median

temp2 <- table(as.vector(MPG))  # calculate Mode
temp2
names(temp2)[temp2 ==max(temp2)]
      # (OR)
names(table(Cars$MPG))[table(Cars$MPG)==max(table(Cars$MPG))] # calculate Mode using second Method

# Second Business Moment

var(Cars$MPG) # Calculate variance

sd(Cars$MPG)  # calculat Standard Deviation

x2<-max(Cars$MPG) # calculate Range
x2
y2<-min(Cars$MPG)
y2
r2<- x2-y2
r2


# Third and Fourth Business Moment

skewness(Cars$MPG) # Negative Skewness

kurtosis(Cars$MPG) # Negative Kurtosis

# Graphical Representation

plot(Cars$MPG) # plot graph
barplot(Cars$MPG) # Bar Plot
hist(Cars$MPG) # histogram
dotchart(Cars$MPG) # Dot chart
stem(Cars$MPG) # stem & leaf plot
boxplot(Cars$MPG ,Cars$HP , Cars$VOL , Cars$SP , Cars$WT , main = "box plot for all columns") # There is outliers in 4 columns except one then we have to remove outliers
boxplot(Cars$MPG) #No outliers in out put variable
plot(Cars, col="navy", main="Matrix Scatterplot")

# 2. Scatter plot or Graphical analysis

scatter.smooth(x=Cars$MPG , y=Cars$HP+Cars$VOL+Cars$SP+Cars$WT, main="MPG ~ HP+VOL+SP+WT") # scatterplot
qqplot(Cars$MPG , HP+VOL+SP+WT)
plot(MPG , HP+VOL+SP+WT)
qqnormPlot(Cars$MPG)

# 3. Calculate correlation coefficient

cor(MPG , HP+VOL+SP+WT)
cor(HP ,VOL) # correlation coefficient R <0.85 then these are in poor relationship
cor(Cars$HP ,Cars$SP) # correlataion value R >0.85 then these variables are in strong relationship
cor(Cars$HP ,Cars$WT) # correlation coefficient R <0.85 then these are in poor relationship
cor(Cars$VOL , Cars$SP) #correlation coefficient R <0.85 then these are in poor relationship
cor(Cars$VOL , Cars$WT) # correlataion value R >0.85 then these variables are in strong relationship

# 4. calculate Multiple linear regression

cars_MPG<-lm(MPG ~ HP+VOL+SP+WT)
summary(cars_MPG)
influence.measures(cars_MPG) # view the influence values
cars_MPG1 <- lm(MPG~ HP+VOL+SP+WT , data = Cars[-c(1,9,70,71,74,77,79,80,81) ,])
summary(cars_MPG1)
influence.measures(cars_MPG1) #view the influence values
cars_HS <-lm(HP ~ SP)
summary(cars_HS)
influence.measures(cars_HS)
lm(VOL ~ WT)

#-----------------------------------------------------------
# Multiple Linear Regression using R -pub Code
library(ISLR)
library(MASS)
library(corrplot)  # We'll use corrplot later on in this example too.
library(dplyr)
library(ggplot2)
library(visreg) # This library will allow us to show multivariate graphs.
library(rgl)
library(knitr)
library(scatterplot3d)
attach(Cars)
head(Cars)
str(Cars)
plot(Cars, col="navy", main="Matrix Scatterplot")
require(caTools)
sample = sample.split(Cars,SplitRatio = 0.80) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 = subset(Cars,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(Cars, sample==FALSE) 