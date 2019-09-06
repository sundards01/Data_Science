library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms

# Hierarchical clustering can be divided into two main types: agglomerat?ve and divisive.
# 1. Agglomerative clustering: It's also known as AGNES (Agglomerative Nesting). It works in a bottom-up manner.
# 2. It's also known as DIANA (Divise Analysis) and it works in a top-down manner. The algorithm is an inverse order of AGNES.?
# Step 1 : Data Preparation

data()     # it is showing sample data from R
data("USArrests") # choose the dataset "USArrests from data"
View(USArrests)  # View the data set

# (OR)

df <- USArrests # choose the dataset "USArrests from data"
View(df)  # Vi?w the data

df <- na.omit(df) # To remove any missing value that might be present in the data
View(df) # View tha data after removing any missing values

df1 <-scale(df) # we start by scaling/standardizing the data using the R function scale
head(df1) 

# ?tep: 2 Agglomerative Hierarchical Clustering

d <- dist(df, method = "euclidean") # Dissimilarity matrix

hc1 <- hclust(d, method = "complete" )   # Hierarchical clustering using Complete Linkage

plot(hc1, cex = 0.6, hang = -1)  # Plot the obtained dendro?ram

hc2 <- agnes(df, method = "complete")   # Compute with agnes

hc2$ac  # Agglomerative coefficient

# methods to assess

m <- c( "average", "single", "complete", "ward")

names(m) <- c( "average", "single", "complete", "ward")

# function to compute co?fficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)
#Similar to before we can visualize the dendrogram:
hc3 <- agnes(df, method = "ward")

pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 

# step : 3 Divisive Hierarchi?al Clustering

hc4 <- diana(df)  # compute divisive hierarchical clustering

hc4$dc  # Divise coefficient; amount of clustering structure found

pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana") ## plot dendrogram

# Step: 4 Working with Dend?ogram

hc5 <- hclust(d, method = "ward.D2" ) #  Ward's method

sub_grp <- cutree(hc5, k = 4)  # Cut tree into 4 groups

table(sub_grp) # Number of members in each cluster

USArrests %>%
  mutate(cluster = sub_grp) %>%
  head

plot(hc5, cex = 0.6) # Dispaly?the Dendrogram

rect.hclust(hc5, k = 4, border = 2:5) # dispaly the clusters with boarders

fviz_cluster(list(data = df, cluster = sub_grp)) # e can also use the fviz_cluster function from the factoextra package to visualize the result in a scatter plot.

?# To use cutree with agnes and diana you can perform the following:
# Cut agnes() tree into 4 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 4)

# Cut diana() tree into 4 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 4)

#Last?y, we can also compare two dendrograms. Here we compare hierarchical clustering with complete linkage versus Ward's method. The function tanglegram plots two dendrograms, side by side, with their labels connected by lines.

# Compute distance matrix
res.di?t <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegr?m(dend1, dend2)

dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_bran?hes = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)

fviz_nbclust(df, FUN = hcut, method = "wss")   # Elbow Method

fviz_nbclust(df, FUN = hcut, method = "silhouette") # Average Silhouette Met?od

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50) # Gap Statistic Method

fviz_gap_stat(gap_stat)


