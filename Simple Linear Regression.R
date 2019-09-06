View(wc_at)
# Calaculate EDA

basicStats(wc_at)
summary(wc_at)

# Graphical Exploration

dotchart(wc_at$Waist)
dotchart(wc_at$Waist, main="Dot Plot of Waist Circumferences")
dotplot(wc_at$Waist)
dotchart(wc_at$AT, main="Dot Plot of Adipose Tissue Areas")
dotplot(wc_at$AT)

# Scatter Plot
attach(wc_at)

plot(Waist,AT,main = "scatter plot for SLR")

plot(wc_at$Waist , wc_at$AT)

boxplot(wc_at$Waist , wc_at$AT)

# Calculate Correlation Coefficient

cor(wc_at$Waist , wc_at$AT)

# Calculate Linear Regression
reg <- lm(AT ~ Waist , data = wc_at)
summary(reg)
confint(reg,level = 0.95)
predict(reg,level = "predict")

reg_log<-lm(AT ~ log(Waist), data = wc_at)
summary(reg_log)
confint(reg_log,level = 0.95)
predict(reg_log , level = "predict")

reg_exp<-lm(log(AT) ~ Waist , data = wc_at)
summary(reg_exp)
confint(reg_exp , level = 0.95)
predict(reg_exp , level = "predict")