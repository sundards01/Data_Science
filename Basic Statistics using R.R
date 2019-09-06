# 1. First Business Moment -Calaculate Mean ,Median and Mode

View(mba)

# Remove the first Column as it is not required for Analysis

mba1 <- mba[ , 2:3] # Remove First Column from original dataset
View(mba1)

mean(mba1$gmat) # Calculate Mean

median(mba1$gmat) # Calculate Median

mode <-names(table(mba1$gmat))[table(mba1$gmat) == max(table(mba1$gmat))] # Calculate Mode
mode

# 2. Second Business Moment - Calculate Variance , Standard Deviation and Range

var(mba1$gmat) # Calculate Varinace

sd(mba1$gmat) # Calculate Standard Deviation

# Calcualte Range = Max-Min

x<-max(mba1$gmat)#Calculate Maximum Value
x

y<-min(mba1$gmat) #Calcualte Minimum Value
y

r1 <- x-y #Calculate range
r1

(OR)

range(max(mba1$gmat) - min(mba1$gmat))

# 3. Third Business Moment -Skewness of the Data .Need to enable e1071 package

skewness(mba1$gmat) #calculate skewness and it is in -Ve sign then it is Negative Skewness

# 4. Fourth Business Moment - Kurtosis of the data

kurtosis(mba1$gmat) #calculate kurtosis and it is in +Ve sign then it is positive kurtosis

# 5. Graphical Representation 

plot(mba1$gmat) # Plot Graph

barplot(mba1$gmat) #Barplot Graph

qqnorm(mba1$gmat) # Q-Q Plot Graph

hist(mba1$gmat) #Histogram graph

pie(mba1$gmat) # Pie graph

boxplot(mba1$gmat) # Box Plot Graph and identifed outliers in the data

# 6. Calculate all stastical functions using the bascistats by enabling the package fbasics

basicStats(mba1$gmat)

# 7. Calculate outliers using IQR = Q3-Q1 , Lowvalue = Q1-1.5*IQR ,MaxValue = Q3+1.5*IQR

summary(mba1$gmat)

# in this summary ,we have identified Q1 = 690 Q3 = 730

Q1 = 690
Q3 = 730

IQR = Q3 - Q1 #calculate IQR
IQR

lowvalue <- Q1 - 1.5 * IQR #calculate lower  value
lowvalue

highvalue <- Q3 + 1.5 * IQR #calculate higher  value
highvalue

# Outliers are lying in the range of lesss than 630 and greater than 790

# 8. We can get actual outliers using the below

boxplot(mba1$gmat)$out # Actual outliers are 600,610 and 620

mba1[which(mba1$gmat %in% outliers),] # First we need to find in which rows the outliers are

mba1_actual <- mba1[-which(mba1$gmat %in% outliers) ,] #remove the rows containing the outliers
mba1_actual
View(mba1_actual)

boxplot(mba1_actual$gmat)

# 9. Calcualte Confidence level on the given data

qnorm(0.90) #Confident level @ 90%
qnorm(0.95) # Confident level @ 95%
qnorm(0.99) # Confident level @ 99%

qt(0.90 , 711) 
qt(0.95 , 711)
qt(0.99 , 711)

quantile(mba1_actual$gmat , 0.95) # Confident level @ 95%
quantile(mba1_actual$gmat , 0.90) # Confident level @ 90%
quantile(mba1_actual$gmat , 0.99) # Confident level @ 99%



