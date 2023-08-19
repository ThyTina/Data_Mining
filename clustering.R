#load library
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(dplyr)
library(corrplot)
library(factoextra)
library(fpc)


# CLUSTERING:
# 1. PCA & Kmeans
# 2. PCA & HIERARCHICAL
# 3. PCA & DBSCAN


# ===== Read data =====
setwd('D:\\')
data <- read.csv(file ='data_processed.csv')
head(data)

summary(data)

# ===== PCA =====

# Load required library
library(stats)

# Perform PCA with 2 components
pca_2 <- prcomp(data, retx = TRUE, rank = 2)

# Extract the reduced data with 2 principal components
reduced_data <- as.matrix(pca_2$x)
reduced_data

# PLOT THE COMPONENTS
# Convert the reduced_data matrix to a data frame
reduced_data_df <- as.data.frame(reduced_data)

# Create a scatter plot of the reduced data
ggplot(data = reduced_data_df, aes(x = PC1, y = PC2)) +
  geom_point() +
  labs(x = "Principal Component 1", y = "Principal Component 2", title = "PCA - Reduced Data Visualization")


# ===== PCA & Kmeans  =====

####### Determine number of clusters
# ====== Select k on reduced data
# wss cho ca bo du lieu (1 cum, 2 thuoc tinh)
wss2 = (nrow(reduced_data) -1)*sum(apply(reduced_data, 2, var))

# k clusters
for (i in 1:10) wss2[i] = sum(kmeans(reduced_data,
                                    centers=i,
                                    nstart=20)$withinss)

wss2

for (i in 1:10) {
  wss2[i] = sum(kmeans(reduced_data,
                       centers = i,
                       nstart = 20)$withinss)
  cat("Number of clusters (k):", i, "\tWSS:", wss2[i], "\n")
}

# Plot wss
plot(1:10, wss2, type='b', xlab="Number of cluster",
     ylab="WSS")

# => select k=4


# buil model su dung kmeans
set.seed(20)
k.means.fit <- kmeans(reduced_data, centers = 4) # k = 4

# Cluster
cluster_assignments <- k.means.fit$cluster
cluster_assignments

# Cluster size
cluster_size <- k.means.fit$size
cluster_size
# [1] 3529 1125 3259 1221

# Compute cluster statistics
library(factoextra) # Make sure you have this library installed
km_stats <- cluster.stats(dist(reduced_data), cluster_assignments)
km_stats

# Within-Cluster Sum of Squares (WCSS)
# $within.cluster.ss
# [1] 10697.5

# Silhouette Score
# $avg.silwidth
# [1] 0.6406251


fviz_cluster(k.means.fit, geo= "point",
             data = reduced_data) + ggtitle("k=4")

# plotcluster(reduced_data, cluster_assignments)

# ===== EVALUATE KMEAN
# Compute the dissimilarity matrix
diss_matrix <- dist(reduced_data)
# Compute silhouette values Kmeans
silhouette_data <- silhouette(k.means.fit$cluster, diss_matrix)
silhouette_avg <- summary(silhouette_data)$avg.width
silhouette_avg
# [1] 0.6406251


# ===== PCA & HIERARCHICAL  =====

d <- dist(reduced_data, method = "euclidean")
H.fit <- hclust(d, method="centroid")
plot(H.fit) # display dendogram
groups <- cutree(H.fit, k=4)
groups

# draw dendogram with red borders around the 4 clusters
rect.hclust(H.fit, k=4, border="red") 

# Compute cluster statistics
km_stats2 <- cluster.stats(dist(reduced_data), groups)
km_stats2

# $within.cluster.ss
# [1] 22663.27

# $avg.silwidth
# [1] 0.5491875


# ====== EVALUATE HIERARCHICAL
# Compute the silhouette values
silhouette_data <- silhouette(groups, d, keep.data = TRUE)
# Calculate the average silhouette width
silhouette_avg <- mean(silhouette_data[, "sil_width"])
silhouette_avg
# [1] 0.5491875


# ===== PCA & DBSCAN  =====
# Compute DBSCAN clustering
dbscanOutput <- dbscan(reduced_data, eps = 0.5, MinPts = 4)

plotcluster(reduced_data, dbscanOutput$cluster)

# Convert DBSCAN clustering result to a factor (or character) vector
dbscan_groups <- as.numeric(dbscanOutput$cluster)

# Compute cluster statistics using silhouette analysis
km_stats3 <- cluster.stats(dist(reduced_data), dbscan_groups)
km_stats3

# $within.cluster.ss
# [1] 2734.483

# $avg.silwidth
# [1] 0.6162586

# ===== EVALUATE DBSCAN
# Compute silhouette values for DBSCAN
silhouette_data <- silhouette(dbscanOutput$cluster, as.dist(dist(reduced_data)))

# Calculate the average silhouette width
silhouette_avg <- mean(silhouette_data[, "sil_width"])
silhouette_avg
# [1] 0.6162586









