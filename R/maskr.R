#' Adding random error to masks
#' @param x a vector of a mask or a matrix containing multiple masks as rows.
#' @param tilt_err determining the SD of error added to tilt angle in degrees.
#' Its default value is 0.1 degrees.
#' @param twist_err is the SD of error added to twist angle in degrees. Its
#' default value is 1.
#' @param mask_err is the SD of error added to mask value. Its default value is 1.
#' @param orientation_err is the SD error added to the orientation angle, with a
#' default of 5 degree.
#' @param az_cover the azimuth coverage of mask, with a default value of 360.
#' @param clipLim a set of lower and upper bounds for masks with no limits as default.
#' @param clipValNA a logical value; TRUE means use NA when mask is clipped; default is FALSE
maskr <- function(x # True Mask
                  ,tilt_err=0.1 # N(0, Tilt error (SD)) in degrees
                  ,twist_err=1 # N(0, twist angle (SD)) in degrees / Or uniform(-180, 180)
                  ,mask_err=1 # N(0, mask_err (SD)) in degrees
                  ,orientation_err=5 # standard deviation in degrees
                  ,az_cover=360 # The azimuth coverage of mask in degrees
                  ,clipLim=c(-Inf, +Inf) # Values for clipping masks
                  ,clipValNA = FALSE # If clipped masks should be treated as NAs
){
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
  phShift <- round(runif(m, 0, 1)*360/nAz, 0)
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
  list(masks=x, tiltDeg=tiltAngle, twistDeg=twistAngle, phShift=phShift)
}
