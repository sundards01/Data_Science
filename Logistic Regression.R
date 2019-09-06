# Calculate Column Count using Table and Data Frame.

a<- table(claimants$CLMSEX)
a

b<-table(claimants$ATTORNEY)
b

c<-table(claimants$CLMINSUR)
c

d<-table(claimants$SEATBELT)
d

temp1<-as.data.frame(table(claimants$ATTORNEY))
temp1

temp2<-as.data.frame((table(claimants$CLMSEX)))
temp2

temp3<-as.data.frame(table(claimants$CLMINSUR))
temp3

temp4<-as.data.frame(table(claimants$SEATBELT))
temp4

# Apply Linear Regression

reg <- lm(claimants$ATTORNEY~ factor(claimants$CLMSEX)+factor(claimants$CLMINSUR)+factor(claimants$SEATBELT)+claimants$CLMAGE+claimants$LOSS)
summary(reg)

# Apply Logistic regression

reg1<-glm(claimants$ATTORNEY~factor(claimants$CLMSEX)+factor(claimants$CLMINSUR)+factor(claimants$SEATBELT)+claimants$CLMAGE+claimants$LOSS , family = binomial , data = claimants)
summary(reg1)

# Removing Negative Values using exponetial

exp(coef(reg1))

# Confusion Matrix

reg2 <- predict(reg1,type = c("response"),claimants)
reg2
confusion <- table(reg2>0.5 ,claimants$ATTORNEY)
confusion

# Calculate Accuracy of Data
Accuracy <- sum(diag(confusion) / sum(confusion))
Accuracy

# Replace the missing values in column CLIAMAGE using Mean

claimants$CLMAGE <- ifelse(is.na(claimants$CLMAGE), ave(claimants$CLMAGE , FUN = function(x) mean(x,na.rm = TRUE)), claimants$CLMAGE)
claimants$CLMAGE

c<-table(claimants$CLMAGE)
c
View(claimants)
mean(claimants$CLMAGE)
summary(claimants)


# Save this file into excel
write.xlsx(claimants ,file = "16-jun-19.xlsx")
getwd()
