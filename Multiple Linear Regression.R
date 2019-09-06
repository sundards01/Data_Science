View(Cars)

# Calculate EDA

basicStats(Cars)

# Graphical Exploration

plot(Cars , main = "Plot of Cars")

boxplot(Cars , main = "Outliers for Cars")

# Calculate Correlation Coefficient

cor(Cars$HP , Cars$VOL)
cor(Cars$HP , Cars$SP)
cor(Cars$HP , Cars$WT)
cor(Cars$VOL ,Cars$SP)
cor(Cars$VOL , Cars$WT)
cor(Cars$MPG , Cars$HP+Cars$VOL+Cars$SP+Cars$WT)
cor(Cars)

# Calculate Regression
attach(Cars)
model.car <- lm(MPG ~ SP+VOL+WT+HP)
summary(model.car)
confint(model.car ,level = 0.95)
predict(model.car , level = "predict")

model.carv <- lm(MPG ~ VOL)
summary(model.carv)
confint(model.carv ,level = 0.95)
predict(model.carv , level = "predict")

model.cars <-lm(MPG ~ SP)
summary(model.cars)
confint(model.cars ,level = 0.95)
predict(model.cars , level = "predict")

model.carw <- lm(MPG ~ WT)
summary(model.carw)
confint(model.carw ,level = 0.95)
predict(model.carw , level = "predict")

model.carh <- lm(MPG ~ HP)
summary(model.carh)
confint(model.carh ,level = 0.95)
predict(model.carh , level = "predict")

model.carvw <- lm(MPG ~ VOL+WT)
summary(model.carvw)

influence.measures(model.car)
