#' Cleaning up the clusters generated using PCAs
#'
library(Math502B)
# 0. Load Data ####
## Load mask2m
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')
str(mask2m)

nRow <- dim(mask2m)[1]
dim(mask2m) <- c(prod(dim(mask2m)[1:2]), dim(mask2m)[3])

pos <- k2ij(nRow, 1:dim(mask2m)[1])

# 3. Choose a Cleaner Cluster ####
## Use Kmean on PCA's, X, Y to create clusters
set.seed(123)
pcaMsk <- prcomp(mask2m)
pca_spree(pcaMsk, xlim=c(1, 10))

summary(pcaMsk)$importance[,1:10]

## 6 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:10], centers = 6)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

# Cleaning the clusters based on position
posCenters <- t(sapply(1:6, function(k){
  apply(pos[kmCluster$cluster==k, ], 2, median)
}))
mdCluster <- apply(pos, 1, function(v){
  vkDist <- sapply(1:6, function(k) sum((v-posCenters[k,])^2))
  which.min(vkDist)
})
plot(ij[,1], ij[,2], col=mdCluster)


write.csv(mdCluster, file = '2DMasks_6CleanClusters.csv', row.names = FALSE)

mask6Cluster <- kmCluster$cluster
save(mask2m, mask6Cluster, mdCluster, file = 'MaskData6Cluster.rda')



if(FALSE){
  kNN(posScaled, k = 6, )
  ## Model Based Clustering: 9 Clusters
  library(mclust)
  mbCluster <- Mclust(pcaMsk$x[,1:10])
  plot(ij[,1], ij[,2], col=mbCluster$classification)


  ## Density based Clustering
  #install.packages("fpc")
  #install.packages("dbscan")
  library(fpc)
  library(dbscan)
  dbCluster <- fpc::dbscan(pcaMsk$x[,1:10], eps = .15)
  ### Did not produce anything: all classes 0



}
