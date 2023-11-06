#' Random Mask Generator

k2ij <- function(nx, k){
  cbind((k-1)%%nx+1, (k-1)%/%nx+1)
}

shiftK <- function(x, K){
  n <- length(x)
  K <- K %% n
  if(K==0) x else
    if(K>0) x[c((1+K):n, 1:K)] else
      if(K<0) x[c((n-K+1):n, 1:(n-K))]
}

rMask <- function(x # True Mask
                  ,tilt_err=0.1 # N(0, Tilt error (SD)) in degrees
                  ,twist_err=1 # N(0, twist angle (SD)) in degrees / Or uniform(-180, 180)
                  ,mask_err=1 # N(0, mask_err (SD)) in degrees
                  ,orientation_err=5 # standard deviation in degrees
                  ,az_cover=360 # The azimuth coverage of mask in degrees
                  ,clipLim=c(-Inf, +Inf) # Values for clipping masks
                  ,clipValNA = FALSE # If clipped masks should be treated as NAs
){
  #browser()
  if(!is.matrix(x)){
    x <- matrix(x, nrow=1)
  }
  # Number of masks
  m <- dim(x)[1]
  # Number of points on one mask
  nAz <- dim(x)[2]

  # Azimuth angles
  az <- seq(0, az_cover/360*2*pi, length.out = nAz)

  # Adding orientation error
  phShift <- round(rnorm(m, 0, orientation_err)*360/nAz, 0)
  x <- lapply(1:m, \(k) shiftK(x[k,], phShift[k]))
  x <- do.call('rbind', x)
  # Adding tilt and noise
  tiltAngle <- rnorm(m, 0, tilt_err)
  twistAngle <- if(twist_err==-1) runif(m, -180, 180) else rnorm(m, 0, twist_err/180*pi)
  noise_twist <- matrix(twistAngle, nrow=m) %*% matrix(sin(az), nrow=1)
  tilt_twist <- lapply(twistAngle, \(a) cos(az-a))
  tilt_twist <- do.call('rbind', tilt_twist)
  noise_tilt=diag(tiltAngle, nrow = m) %*% tilt_twist - noise_twist
  noise_mask=matrix(rnorm(m*nAz)*mask_err, nrow = m)
  out <- x+noise_tilt+noise_mask
  # Limit
  out[which(out<clipLim[1])] <- if(clipValNA) NA else clipLim[1]
  out[which(out>clipLim[2])] <- if(clipValNA) NA else clipLim[2]
  #
  list(masks=out, tiltDeg=tiltAngle, twistDeg=twistAngle, phShift)
}

# Solving the Memory issue
rMaskmmm <- function(x # True Mask
                  ,tilt_err=0.1 # N(0, Tilt error (SD)) in degrees
                  ,twist_err=1 # N(0, twist angle (SD)) in degrees / Or uniform(-180, 180)
                  ,mask_err=1 # N(0, mask_err (SD)) in degrees
                  ,orientation_err=5 # standard deviation in degrees
                  ,az_cover=360 # The azimuth coverage of mask in degrees
                  ,clipLim=c(-Inf, +Inf) # Values for clipping masks
                  ,clipValNA = FALSE # If clipped masks should be treated as NAs
){
  #browser()
  if(!is.matrix(x)){
    x <- matrix(x, nrow=1)
  }
  # Number of masks
  m <- dim(x)[1]
  # Number of points on one mask
  nAz <- dim(x)[2]

  # Azimuth angles
  az <- seq(0, az_cover/360*2*pi, length.out = nAz)

  # Adding orientation error
  phShift <- round(rnorm(m, 0, orientation_err)*360/nAz, 0)
  x <- lapply(1:m, \(k) shiftK(x[k,], phShift[k]))
  x <- do.call('rbind', x)
  # Adding tilt and twist
  tiltAngle <- rnorm(m, 0, tilt_err)
  twistAngle <- if(twist_err==-1) runif(m, -180, 180) else rnorm(m, 0, twist_err/180*pi)
  # Adding noise
  x <- lapply(
    1:m,
    function(k){
      v <- shiftK(x[k,], phShift[k])
      noise_tilt <- tiltAngle[k]*cos(az-twistAngle[k]) - twistAngle[k]*sin(az)
      noise_mask <- rnorm(nAz)*mask_err
      v + noise_tilt + noise_mask
    })
  x <- do.call('rbind', x)

  # Limit
  x[which(x<clipLim[1])] <- if(clipValNA) NA else clipLim[1]
  x[which(x>clipLim[2])] <- if(clipValNA) NA else clipLim[2]
  #
  list(masks=x, tiltDeg=tiltAngle, twistDeg=twistAngle, phShift)
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
