#' Creating Random Masks with Mean Clusters [and Cleaned]
#'
rm(list = ls())
library(Math502B)

if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')

source('rMaskGen.R')

# Mean-based Clusters
nRow <- dim(mask2m)[1]
dim(mask2m) <- c(prod(dim(mask2m)[1:2]), dim(mask2m)[3])
meanArr <- apply(mask2m, 1, mean)

kClusters <- sapply(1:9, \(k) with(kmeans(meanArr, k), tot.withinss/totss))

plot(1-kClusters, type='h', main='SSE/SST for Number of Clusters')
lines(1-kClusters, type='l', col='red')
which(1-kClusters>.95)[1]


mask8Cluster <- kmeans(meanArr, 8)

#
m <- dim(mask2m)[1]
pos <- k2ij(304, 1:m)

plot(pos[,1], pos[,2], col=mask8Cluster$cluster)


# Mean and SD Clustering
# Mean-based Clusters
sdArr <- apply(mask2m, 1, sd)

kClusters <- sapply(1:15, \(k)
                    with(kmeans(cbind(meanArr, sdArr), k),
                         tot.withinss/totss))

plot(1-kClusters, type='h', main='SSE/SST for Number of Clusters')
lines(1-kClusters, type='l', col='red')
which(1-kClusters>.90)[1]


mask6Cluster <- kmeans(cbind(meanArr, sdArr), 6)

plot(pos[,1], pos[,2], col=mask6Cluster$cluster)

# Mean, Sd, X & Y
kClusters <- sapply(
  1:10, \(k)
  with(
    kmeans(scale(cbind(meanArr, sdArr, pos)), k),
    tot.withinss/totss))

plot(1-kClusters, type='h', main='SSE/SST for Number of Clusters')
lines(1-kClusters, type='l', col='red')
which(1-kClusters>.90)[1]


mask4Cluster <- kmeans(scale(cbind(meanArr, sdArr, pos)), 4)

plot(pos[,1], pos[,2], col=mask4Cluster$cluster)


# Mean, X & Y
kClusters <- sapply(
  1:10, \(k)
  with(
    kmeans(scale(cbind(meanArr, pos)), k),
    tot.withinss/totss))

plot(1-kClusters, type='h', main='SSE/SST for Number of Clusters')
lines(1-kClusters, type='l', col='red')
which(1-kClusters>.90)[1]


mask8Cluster <- kmeans(scale(cbind(meanArr, pos)), 8)

plot(pos[,1], pos[,2], col=mask8Cluster$cluster)


# Mean, X & Y
params <- c(0, .01, .05, .1, .5, 1)
par(mfrow=c(length(params), 2), mgp=c(.5, .5, 0), xpd=TRUE, mar=c(1,1,1,0)+.1)
for(xyf in params){
  kClusters <- sapply(
    1:10, \(k)
    with(
      kmeans(scale(cbind(meanArr, pos))%*% diag(c(1, xyf, xyf)), k),
      tot.withinss/totss))

  plot(1-kClusters, type='h',
       main='R-Sq for Number of Clusters', cex.main=.7,
       cex.axis=.5, xlab='', ylab='')
  lines(1-kClusters, type='l', col='red')
  which(1-kClusters>.90)[1]

  k <- 5
  kMaskCluster <- kmeans(scale(cbind(meanArr, pos))%*%
                         diag(c(1, xyf, xyf)),
                         k)
  cs <- round(kMaskCluster$centers[,1], 2)
  ics <- order(cs)
  plot(pos[,1], pos[,2], col=ics[kMaskCluster$cluster],
       main=sprintf('(Mean, X, Y): %g Clusters, XY-factor=%g', k, xyf),
       xlab='', ylab='', cex.axis=.5, cex.main=.7)
  legend('right', legend = cs[ics],
         cex = .6, text.col = 1:5)
}

# Mean, X & Y: SVM
library(e1071)
params <- c(0, .01, .05, .1, .5, 1)
par(mfrow=c(length(params), 2), mgp=c(.5, .5, 0), xpd=TRUE, mar=c(1,1,1,0)+.1)
for(xyf in params){
  kClusters <- sapply(
    1:10, \(k)
    with(
      kmeans(scale(cbind(meanArr, pos))%*% diag(c(1, xyf, xyf)), k),
      tot.withinss/totss))

  plot(1-kClusters, type='h',
       main='R-Sq for Number of Clusters', cex.main=.7,
       cex.axis=.5, xlab='', ylab='')
  lines(1-kClusters, type='l', col='red')
  which(1-kClusters>.90)[1]

  k <- 5
  kMaskCluster <- kmeans(scale(cbind(meanArr, pos))%*%
                           diag(c(1, xyf, xyf)),
                         k)
  cs <- round(kMaskCluster$centers[,1], 2)
  ics <- order(cs)
  plot(pos[,1], pos[,2], col=ics[kMaskCluster$cluster],
       main=sprintf('(Mean, X, Y): %g Clusters, XY-factor=%g', k, xyf),
       xlab='', ylab='', cex.axis=.5, cex.main=.7)
  legend('right', legend = cs[ics],
         cex = .6, text.col = 1:5)
}


## generate 10 random sets
nrnd <- 10
k <- nrnd * m

C <- rep(mask6Cluster, nrnd)
CleanC <- rep(mdCluster, nrnd)
Y <- rep(1:m, nrnd)
X <- matrix(0, nrow = k, ncol = dim(mask2m)[2])
tiltDeg <- rep(0, k)
twistDeg <- rep(0, k)
phShift <- rep(0, k)

for(i in 1:nrnd){
  kMask <- rMask(mask2m, tilt_err = .1, twist_err = 5, mask_err = 2)
  X[(1:m)+(i-1)*m,] <- kMask$masks
  tiltDeg[(1:m)+(i-1)*m] <- kMask$tiltDeg
  twistDeg[(1:m)+(i-1)*m] <- kMask$twistDeg
  phShift[(1:m)+(i-1)*m] <- kMask$phShift
}

loss <- function(y, yhat, method=c("distance", "mismatch"), xy=pos){
  mthd <- match.arg(method[1], c("distance", "mismatch"))
  if(length(mthd)==0)
    stop('Use one of "distance" or "match" as loss calculation method.')
  if(mthd=="distance"){
    rowSums((xy[y,, drop=FALSE]-xy[yhat,,drop=FALSE])^2)
  }else {
    1.0*(y!=round(yhat,0))
  }
}
maskData <- list(x=X, y=Y, class=C, cleanClass=CleanC, pos=pos, loss=loss,
                 tiltDeg=tiltDeg, twistDeg=twistDeg, phShift=phShift)
save(maskData, file = 'NoisyData10-6Cluster.rda')
rm(X, Y, C, CleanC, phShift, twistDeg, tiltDeg)
write.csv(maskData$x, 'Noisy10Data.csv', row.names = FALSE)
write.csv(maskData$y, 'Noisy10Data_y.csv', row.names = FALSE)
write.csv(maskData$class, 'Noisy10Data_class.csv', row.names = FALSE)
write.csv(maskData$cleanClass, 'Noisy10Data_cleanClass.csv', row.names = FALSE)
write.csv(maskData$tiltDeg, 'Noisy10Data_tilt.csv', row.names = FALSE)
write.csv(maskData$twistDeg, 'Noisy10Data_twist.csv', row.names = FALSE)
write.csv(maskData$phShift, 'Noisy10Data_shift.csv', row.names = FALSE)
write.csv(pos, 'NoisyDataPosition.csv', row.names = FALSE)
rm(pos, maskData, kMask, mask2m, mask6Cluster, mdCluster)

