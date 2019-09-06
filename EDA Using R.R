View(mba)

# First Business Moment Decision

mean(mba$workex)
mean(mba$gmat)
median(mba$workex)
median(mba$gmat)

temp<-table(as.vector(mba$gmat))
temp
names(temp)[temp == max(temp)]

# Second Business Moment Decision

var(mba$workex)
var(mba$gmat)
sd(mba$workex)
sd(mba$gmat)
x<-max(mba$gmat)
x
y<-min(mba$gmat)
y
r1<-x-y
r1

range(max(mba$gmat) - min(mba$gmat))

# Third & Fourth Business Moment Decision

skewness(mba$gmat)
kurtosis(mba$gmat)

# Various Graphical Representation

plot(mba$gmat)
barplot(mba$gmat)
hist(mba$gmat)
boxplot(mba$gmat)

# Calculate Outliers lying between

summary(mba$gmat)
Q1<-690
Q1
Q3<-730
Q3
IQR <- Q3-Q1
IQR

lowvalue <- Q1-1.5*IQR
lowvalue

Highvalue <- Q3+1.5*IQR
Highvalue

# Calculate Normalization

qqnorm(mba$gmat)
qqnormPlot(mba$gmat)
qqline(mba$gmat)


# Calculate Confident Level
qnorm(.90)
qnorm(.95)
qnorm(.99)

summary(mba$gmat)
qt(.90,711)
qt(.95,711)
qt(.99,711)