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

write.csv(mask2m, file = "PosComplete-FeatursFull_data.csv", row.names = FALSE)
write.csv(xy, file = "PosComplete-FeatursFull_pos.csv", row.names = FALSE)

#' Find clusters using full response
K <- 10
cAC <- rSq <- rep(0, K) # Autocorrelation and R-Square

for(k in 1:K){
  fullRespCluster <- kmeans(mask2m, centers = k)
  rSq[k] <- 1-fullRespCluster$tot.withinss/fullRespCluster$totss
  cAC[k] <- cor(head(fullRespCluster$cluster, -1), tail(fullRespCluster$cluster, -1))
  cat('\nClusters:', k, '\tR-Square: ', rSq[k], '\tAutoCorrelation: ', cAC[k])
}

par(mfrow=c(2,1), mar=rep(1.5,4), mgp=c(1.2, .4, 0))
plot(rSq)
plot(cAC)


#' Let go with 10 clusters
k <- 10
fullRespCluster <- kmeans(mask2m, centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

write.csv(fullRespCluster$cluster, file = "PosComplete-FeatursFull_Kmean10.csv", row.names = FALSE)
write.csv(fullRespCluster$centers, file = "PosComplete-FeatursFull_Kmean10Centers.csv", row.names = FALSE)


#' Original 6 clusters
k <- 6
fullRespCluster <- kmeans(mask2m, centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

write.csv(fullRespCluster$cluster, file = "PosComplete-FeatursFull_Kmean6.csv", row.names = FALSE)
write.csv(fullRespCluster$centers, file = "PosComplete-FeatursFull_Kmean6Centers.csv", row.names = FALSE)

# Including coordinates
K <- 10
cAC <- rSq <- rep(0, K) # Autocorrelation and R-Square

for(k in 1:K){
  fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
  rSq[k] <- 1-fullRespCluster$tot.withinss/fullRespCluster$totss
  cAC[k] <- cor(head(fullRespCluster$cluster, -1), tail(fullRespCluster$cluster, -1))
  cat('\nClusters:', k, '\tR-Square: ', rSq[k], '\tAutoCorrelation: ', cAC[k])
}

par(mfrow=c(2,1), mar=rep(1.5,4), mgp=c(1.2, .4, 0))
plot(rSq)
plot(cAC)

k <- 6
fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])


k <- 6
fullRespCluster <- kmeans(cbind(mask2m, .3+.7*xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

k <- 7
fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])


k <- 10
fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

k <- 10
fullRespCluster <- kmeans(cbind(mask2m, .5+.5*xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])


k <- 6
fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

xyMedPos <- lapply(1:k,
                   \(j) cbind(median(xy[fullRespCluster$cluster==j, 1]),
                              median(xy[fullRespCluster$cluster==j, 2]))
) |> do.call(what='rbind')
points(xyMedPos, pch = 21, col='black', cex=3)

write.csv(fullRespCluster$cluster, file = "PosComplete-FeatursFull_KmeanXY6.csv", row.names = FALSE)
write.csv(fullRespCluster$centers[,1:361], file = "PosComplete-FeatursFull_KmeanXY6Centers.csv", row.names = FALSE)


k <- 10
fullRespCluster <- kmeans(cbind(mask2m, xy), centers = k)
ClusCols <- hsv(h = (1:k)/k, s = 1, v = .8, alpha = .5)
plot(xy[,1], xy[,2], col=ClusCols[fullRespCluster$cluster])

write.csv(fullRespCluster$cluster, file = "PosComplete-FeatursFull_KmeanXY10.csv", row.names = FALSE)
write.csv(fullRespCluster$centers[,1:361], file = "PosComplete-FeatursFull_KmeanXY10Centers.csv", row.names = FALSE)



#' Find closest points to the center
dCols <- colorRampPalette(colors = c(adjustcolor('tomato', .5),
                                     adjustcolor('skyblue', .2)))(101)


#' C = 1
C=1
dsig <- mask2m - matrix(1, nrow=dim(mask2m)[1], ncol=1)%*%
  fullRespCluster$centers[C, , drop = FALSE]
dsig <- rowSums(dsig^2)
dsig <- log(dsig)/log(max(dsig))
plot(xy[,1], xy[,2], col=dCols[round(dsig*100, 0)+1])
#' C = 2
C=2
dsig <- mask2m - matrix(1, nrow=dim(mask2m)[1], ncol=1)%*%
  fullRespCluster$centers[C, , drop = FALSE]
dsig <- rowSums(dsig^2)
dsig <- log(dsig)/log(max(dsig))
plot(xy[,1], xy[,2], col=dCols[round(dsig*100, 0)+1])

#' C = 3
C=3
dsig <- mask2m - matrix(1, nrow=dim(mask2m)[1], ncol=1)%*%
  fullRespCluster$centers[C, , drop = FALSE]
dsig <- rowSums(dsig^2)
dsig <- log(dsig)/log(max(dsig))
plot(xy[,1], xy[,2], col=dCols[round(dsig*100, 0)+1])


#

