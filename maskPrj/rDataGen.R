# Generating noisy data
# Load True Masks
if(file.exists('mask2m.rda')){
  load('mask2m.rda')
} else{
  library(R.matlab)
  x <- readMat(file.choose())
  mask2m <- x$mask2m
  save(mask2m, file = 'mask2m.rda')
}

setwd('maskPrj')
source('rMaskGen.R')
mdim <- dim(mask2m)
k <- prod(mdim[1:2])
pos <- k2ij(mdim[1], 1:k)
x <- mask2m
dim(x) <- c(k, mdim[3])

nrnd <- 10
Y <- rep(1:k, nrnd)
X <- x
for(i in 2:nrnd){
  X <- rbind(X, rMask(x, tilt_err = .1, mask_err = 1))
}

rm(x, mask2m)
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
maskData <- list(x=X, y=Y, pos=pos, loss=loss)
save(maskData, file = 'MaskData.rda')
