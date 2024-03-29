# The code below will simply load the data and name all 32 variables.

features <- c("radius", "texture", "perimeter", "area", "smoothness", "compactness", "concavity", "concave_points", "symmetry", "fractal_dimension")

names(wbcd) <- c("id", "diagnosis", paste0(features,"_mean"), paste0(features,"_se"), paste0(features,"_worst"))

wbcd.pr <- prcomp(wbcd[c(3:32)], center = TRUE, scale = TRUE)
summary(wbcd.pr)

screeplot(wbcd.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(wbcd.pr$sdev^2 / sum(wbcd.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),col=c("blue"), lty=5, cex=0.6)

plot(wbcd.pr$x[,1],wbcd.pr$x[,2], xlab="PC1 (44.3%)", ylab = "PC2 (19%)", main = "PC1 / PC2 - plot")

library("factoextra")
fviz_pca_ind(wbcd.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = wbcd$diagnosis, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Diagnosis") +
  ggtitle("2D PCA-plot from 30 feature dataset") +
  theme(plot.title = element_text(hjust = 0.5))