# 1. Exploratory Data Analysis on Given Data

# First Business Moment

mean(wc_at$Waist) # Mean
mean(wc_at$AT)  # Mean

median(wc_at$Waist) #Median
median(wc_at$AT)    # Median

temp1 <- table(as.vector(wc_at$Waist)) # Calculate Mode
temp1
names(temp1)[temp1 == max(temp1)]

names(table(wc_at$Waist))[table(wc_at$Waist)==max(table(wc_at$Waist))] # Calculate Mode
names(table(wc_at$AT))[table(wc_at$AT) == max(table(wc_at$AT))] # Calculate Mode

# Second Business Moment

var(wc_at$Waist) # Calculate Variance
var(wc_at$AT)    # calculate variance

sd(wc_at$Waist) # calculate standard deviation
sd(wc_at$AT)    # calculate standard deviation

x<- max(wc_at$Waist) # Calculate range
x
y<- min(wc_at$Waist)
y
r<-x-y
r

x1<-max(wc_at$AT)   # Calculate range
x1
y1<- min(wc_at$AT)
y1
r1<-x1-y1
r1
# Third and Fourth business moment

skewness(wc_at$Waist) # Positive skewness
skewness(wc_at$AT)    # Positive skewness

kurtosis(wc_at$Waist)  # negating Kurtosis
kurtosis(wc_at$AT)     # negating Kurtosis

# Graphical Representation

hist(wc_at$Waist)
hist(wc_at$AT)

boxplot(wc_at$Waist) # No Outliers in the given data,so no need to delete any data
boxplot(wc_at$AT)    # No Outliers in the given data,so no need to delete any data

par(mfrow = c(1 , 2)) # divide graph area in 2 columns
boxplot(wc_at$Waist, main="Waist", sub=paste("Outlier rows: ", boxplot.stats(wc_at$Waist)$out))  # box plot for Waist
boxplot(wc_at$AT, main="AT", sub=paste("Outlier rows: ", boxplot.stats(wc_at$AT)$out))  # box plot for AT

plot(density(wc_at$Waist), main="Density Plot: Waist", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wc_at$Waist), 2)))   # density plot for skewness Waist
polygon(density(wc_at$Waist), col="red") # fill the colur in density

plot(density(wc_at$Waist), main="Density Plot: Waist", ylab="Frequency", sub=paste("Kurtosis:", round(e1071::kurtosis(wc_at$Waist), 2)))   # density plot for kurtosis Waist
polygon(density(wc_at$Waist), col="red") # fill the colur in density

plot(density(wc_at$AT), main="Density Plot: Waist", ylab="Frequency", sub=paste("Skewness:", round(e1071::skewness(wc_at$AT), 2)))   # density plot for skewness Waist
polygon(density(wc_at$AT), col="blue") # fill the colur in density

plot(density(wc_at$AT), main="Density Plot: Waist", ylab="Frequency", sub=paste("Kurtosis:", round(e1071::kurtosis(wc_at$AT), 2)))   # density plot for kurtosis Waist
polygon(density(wc_at$AT), col="blue") # fill the colur in density

# 2. Scatter Diagram

scatter.smooth(x=wc_at$Waist, y=wc_at$AT, main="AT ~ Waist")  # scatterplot
plot(wc_at$Waist , wc_at$AT)    # Plot
qqplot(wc_at$Waist , wc_at$AT)   # Q-Q Plot


# 3. Calculate the correlation coefficient 

cor(wc_at$Waist , wc_at$AT) # correlatioin coefficient r = 0.81 and it is less than <0.85 %

cor(wc_at$Waist , log(wc_at$AT)) # correlatioin coefficient r = 0.84 and it is less than <0.85 %

cor(log(wc_at$Waist) , wc_at$AT) # correlatioin coefficient r = 0.82 and it is less than <0.85 %

cor(log(wc_at$Waist), log(wc_at$AT)) # correlatioin coefficient r = 0.86 and it is greater than .0.85% then they are in strong relationship

# 4. Calculate regression using lm function

reg <- lm(wc_at$AT ~ wc_at$Waist , data = wc_at) # Calculate regression using lm function
summary(reg)
confint(reg , level = 0.95)
predict(reg , interval = "predict")
AIC(reg) # Calculate akaike information criterion
BIC(reg) # Calculate Bayesian information criterion

reg1 <- lm(wc_at$AT ~ log(wc_at$Waist)) # Calculate regression using Transformation rules
summary(reg1)
confint(reg1, level = 0.95)
predict(reg1 , interval = "predict")
AIC(reg1) # Calculate akaike information criterion
BIC(reg1) # Calculate Bayesian information criterion

reg2<-lm(log(wc_at$AT) ~ wc_at$Waist)   # Calculate regression using Transformation rules
summary(reg2)
confint(reg2 , level = 0.95)
predict(reg2 , interval = "predict")
AIC(reg2) # Calculate akaike information criterion
BIC(reg2) # Calculate Bayesian information criterion

