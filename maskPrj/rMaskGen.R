#' Random Mask Generator

k2ij <- function(nx, k){
  cbind((k-1)%%nx+1, (k-1)%/%nx+1)
}

rMask <- function(x # True Mask
                  ,tilt_err=0.1 # Tilt error
                  ,mask_err=1
                  ,az_cover=2*pi # The azimuth coverage of mask
){
  # Number of points on one mask
  nAz <- if(is.matrix(x)) dim(x)[2] else length(x)
  # Azimuth angles
  az <- seq(0, az_cover, length.out = nAz)
  tiltAngle=rnorm(1, 0, tilt_err)
  noise_tilt=cos(az-runif(1, 0, 2*pi))*tiltAngle
  noise_mask=rnorm(nAz)*mask_err

  if(is.matrix(x)){
    x+matrix(1,nrow = dim(x)[1], ncol = 1)%*%
      matrix(noise_tilt+noise_mask, nrow=1)
  } else
    x+noise_tilt+noise_mask
}

## Example
if(FALSE){
  # Load True Masks
  if(file.exists('mask2m.rda')){
    load('mask2m.rda')
  } else{
    library(R.matlab)
    x <- readMat(file.choose())
    mask2m <- x$mask2m
    save(mask2m, file = 'mask2m.rda')
  }

  str(mask2m)
  tM11 <- mask2m[1,1,];
  rM11 <- rMask(tM11, tilt_err = 1, mask_err = .5, az_cover = 2*pi);
  plot(tM11, cex=.5, col=adjustcolor('red', .3))
  lines(rM11, col=adjustcolor('blue', .5))
  points(rM11, pch='.', cex=2)

}
