#' Reducing Data Size:
#'  1. Thining Data
#'  2. Limiting to a Cluster
#'
library(Math502B)
# 0. Load Data ####
## Load mask2m
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')
str(mask2m)

# 1. Thin Data ####
## Create a smaller data size:
thinMask <- thinIJ(mask2m, 10, returnIJ = FALSE)
str(thinMask)
save(thinMask, file = 'thinMask31x31.rda')

dim(thinMask) <- c(prod(dim(thinMask)[1:2]), dim(thinMask)[3])
str(thinMask)
write.csv(thinMask, file = 'thinMask31x31.csv', row.names = FALSE)


# 2. Choose a cluster ####
## Use Kmean on PCA's to create clusters
nRow <- dim(mask2m)[1]
dim(mask2m) <- c(prod(dim(mask2m)[1:2]), dim(mask2m)[3])
pcaMsk <- prcomp(mask2m)
pca_spree(pcaMsk, xlim=c(1, 10))

summary(pcaMsk)$importance[,1:10]

## 4 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:10], centers = 4)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

## 5 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:10], centers = 5)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)


wss <- rep(0, 12)
for(i in 1:length(wss)){
  wss[i] <- kmeans(pcaMsk$x[, 1:10], centers = i)$tot.withinss[[1]]
}
plot(wss)

## 6 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:10], centers = 6)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

write.csv(mask2m, file = '2DMasks.csv', row.names = FALSE)
write.csv(kmCluster$cluster, file = '2DMasks_6Clusters.csv', row.names = FALSE)

mask6Cluster <- kmCluster$cluster
save(mask2m, mask6Cluster, file = 'MaskData6Cluster.rda')
