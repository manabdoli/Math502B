# Generating noisy data
# Load True Masks

if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(file.exists('mask2m.rda')){
  load('mask2m.rda')
} else{
  library(R.matlab)
  x <- readMat(file.choose())
  mask2m <- x$mask2m
  save(mask2m, file = 'mask2m.rda')
}

source('rMaskGen.R')
mdim <- dim(mask2m)
k <- prod(mdim[1:2])
pos <- k2ij(mdim[1], 1:k)
x <- mask2m
dim(x) <- c(k, mdim[3])
rm(mask2m)

nrnd <- 10
Y <- rep(1:k, nrnd)
X <- x
tiltDeg <- rep(0, k)
twistDeg <- rep(0, k)
phShift <- rep(0, k)

for(i in 2:nrnd){
  kMask <- rMaskmmm(x, tilt_err = .1, twist_err = 5, mask_err = 2,
                 orientation_err = 5)
  X <- rbind(X, kMask$masks)
  tiltDeg <- c(tiltDeg, kMask$tiltDeg)
  twistDeg <- c(twistDeg, kMask$twistDeg)
  phShift <- c(phShift, kMask$phShift)
}

rm(x, kMask)

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
maskData <- list(x=X, y=Y, pos=pos, loss=loss, tiltDeg=tiltDeg,
                 twistDeg=twistDeg, phShift=phShift)
save(maskData, file = 'MaskData.rda')
rm(phShift, twistDeg, tiltDeg, X, Y)
