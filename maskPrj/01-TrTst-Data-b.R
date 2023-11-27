#' Creating Random Masks with Cleaned Clusters
#'
rm(list = ls())
library(Math502B)

# 2 Random Mask for Cluster Data ####
## 2.0 Load Mask
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask6Cluster') || !exists('mask2m')) load('MaskData6Cluster.rda')
str(mask2m)
str(mask6Cluster)
str(mdCluster)

## generate 10 random sets
source('rMaskGen.R')
nrnd <- 10
m <- dim(mask2m)[1]
k <- nrnd * m
pos <- k2ij(304, 1:m)

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

