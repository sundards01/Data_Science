View(suicide)
attach(suicide)
str(suicide)
summary(suicide)

#Exploratory Data Analysis

# 1st Business Decision Moment (Central Tendency)

# Calculate Mean
mean(suicides_no)
mean(population)

# Calculate Median
median(suicides_no)
median(population)

# Calculate Mode
temp <- table(as.vector(suicides_no))
temp
names(temp)[temp == max(temp)]

temp1 <- table(as.vector(population))
temp1
names(temp1)[temp1 == max(temp1)]

# 2nd Business Decision Moment (Measure of Dispersion)

# Calculate Variance
var(suicides_no)
var(population)

# Calculate Standard Deviation
sd(suicides_no)
sd(population)

# Calculate Range

max(suicides_no)
min(suicides_no)
range(suicides_no)
r1<-max(suicides_no) - min(suicides_no)
r1

max(population)
min(population)
range(population)
r2 <-max(population)- min(population)
r2

# 3rd Business Decision Moment (Skewness & Kurtoisis)
# For this, we need to install the package e1071

skewness(suicides_no)
skewness(population)
kurtosis(suicides_no)
kurtosis(population)

# View all statistics using the package Fbasics
basicStats(suicides_no)
basicStats(population)

# 4th Business Decision Moment (Graphical Representation)
plot(suicides_no)
barplot(suicides_no)
hist(suicides_no)
boxplot(suicides_no)
plot(population)
barplot(population)
hist(population)
hist(year)

#Calculate Scale

scale(suicides_no)
scale(population)

# Calculate Q-Normalization

qqnorm(suicides_no)
qqnorm(population)
qqnormPlot(suicides_no)
qqnormPlot(population)
qnorm(.90)
qnorm(.95)
qnorm(.99)
qnorm(population)
qqline(suicides_no)
qqline(population)
qt(.90,131)
qt(.95, 131)
qt(.99 , 1486143)

# Calculate Outliers

summary(suicide)
Q3 <- 131
Q3
Q1 <- 3
Q1
IQR <- Q3-Q1
IQR

Q3p <- 1486143
Q3p
Q1p <-97498
Q1p
IQRp <- Q3p - Q1p
IQRp

r1 <- 1.5*IQR
r1
low <- Q1-r1
low
high <- Q3+r1
high

r2 <- 1.5*IQRp
r2
low1<-Q1p-r2
low1
high1<-Q3p+r2
high1

# Remove rows and Columns

suicide1 <- suicide[ , 1]
suicide1

suicide2 <- suicide[ , -1]
suicide2

suicide3 <- suicide2[-1 ,]
suicide3
