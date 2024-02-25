#' Clustering using full data and full features:
#'
library(Math502B)
# 0. Load Data ####
## Load mask2m
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')
str(mask2m)

#' Flatten the data
nRow <- dim(mask2m)[1]
dim(mask2m) <- c(prod(dim(mask2m)[1:2]), dim(mask2m)[3])

xy <- k2ij(nx = 304, 1:dim(mask2m)[1]) |> `colnames<-`(c('x', 'y'))

mask2mPCA <- prcomp(mask2m)
mask2mPCA <- pca_spree(mask2mPCA, xlim=c(0, 15))

pcaK <- sum(mask2mPCA$cumVar<.99)
R <- mask2mPCA$rotation[,1:pcaK]
Y <- x2pca(mask2m, R)
write.csv(R, file = "PosComplete-Featurs6PCA_rotation.csv", row.names = FALSE)
#write.csv(xy, file = "PosComplete-FeatursFull_pos.csv", row.names = FALSE)

#' Find clusters using full response
K <- 30
cAC <- rSq <- rep(0, K) # Autocorrelation and R-Square

cat('\n\nK-Means using 6-PCA:\n')
for(k in 1:K){
  kClusters <- kmeans(Y, centers = k)
  rSq[k] <- 1-kClusters$tot.withinss/kClusters$totss
  cAC[k] <- cor(head(kClusters$cluster, -1), tail(kClusters$cluster, -1))
  cat('\nClusters:', k, '\tR-Square: ', rSq[k], '\tAutoCorrelation: ', cAC[k])
}

par(mfrow=c(2,1), mar=rep(1.5,4), mgp=c(1.2, .4, 0))
plot(rSq)
plot(cAC)


# Including coordinates
K <- 30
cAC <- rSq <- rep(0, K) # Autocorrelation and R-Square

cat('\n\nK-Means using Full-Features+XY:\n')
for(k in 1:K){
  kClusters <- kmeans(cbind(Y, xy), centers = k)
  rSq[k] <- 1-kClusters$tot.withinss/kClusters$totss
  cAC[k] <- cor(head(kClusters$cluster, -1), tail(kClusters$cluster, -1))
  cat('\nClusters:', k, '\tR-Square: ', rSq[k], '\tAutoCorrelation: ', cAC[k])
}

par(mfrow=c(2,1), mar=rep(1.5,4), mgp=c(1.2, .4, 0))
plot(rSq)
plot(cAC)

## The one with position seems to have higher correlation (better)



# Let go with 10 clusters
## Only PCAs
k <- 10
kClusters <- kmeans(Y, centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[kClusters$cluster])
cntrs <- clusterCenter(xy, kClusters$cluster)
text(cntrs, labels=1:dim(cntrs)[1])

write.csv(kClusters$cluster, file = "PosComplete-Featurs6PCA_Kmean10.csv", row.names = FALSE)
write.csv(kClusters$centers[,1:pcaK], file = "PosComplete-Featurs6PCA_Kmean10Centers.csv", row.names = FALSE)


## With Position
kClusters <- kmeans(cbind(Y, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[kClusters$cluster])
cntrs <- clusterCenter(xy, kClusters$cluster)
text(cntrs, labels=1:dim(cntrs)[1])

write.csv(kClusters$cluster, file = "PosComplete-Featurs6PCA_KmeanXY10.csv", row.names = FALSE)
write.csv(kClusters$centers[,1:pcaK], file = "PosComplete-Featurs6PCA_KmeanXY10Centers.csv", row.names = FALSE)




# How about the original 6 clusters
k <- 6
## Only PCAs
kClusters <- kmeans(Y, centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[kClusters$cluster])
cntrs <- clusterCenter(xy, kClusters$cluster)
text(cntrs, labels=1:dim(cntrs)[1])

write.csv(kClusters$cluster, file = "PosComplete-Featurs6PCA_Kmean6.csv", row.names = FALSE)
write.csv(kClusters$centers[,1:pcaK], file = "PosComplete-Featurs6PCA_Kmean6Centers.csv", row.names = FALSE)


## With Position
kClusters <- kmeans(cbind(Y, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[kClusters$cluster])
cntrs <- clusterCenter(xy, kClusters$cluster)
text(cntrs, labels=1:dim(cntrs)[1])

write.csv(kClusters$cluster, file = "PosComplete-Featurs6PCA_KmeanXY6.csv", row.names = FALSE)
write.csv(kClusters$centers[,1:pcaK], file = "PosComplete-Featurs6PCA_KmeanXY6Centers.csv", row.names = FALSE)


