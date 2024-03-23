#' Computing frequency & magnitudes of an FFT
#' @usage freqMeg.vector(fftx)
#' @usage freqMeg.matrix(fftx)
#'
#' @param fftx a vector or matrix of fast-fourier transformation
#' @return a list of three values: mag (magnitude), ang (angle in rad), and freq (frequencies)
#'
#' @export
freqMag.vector <- function(fftx){
  n <- length(fftx)
  if((n%%2)==0){
    # even samples
    freq <- 0:(n/2)
    mag <- fftx[freq+1]/n
    # Scale to return magnitude in time domain
    mag[freq[-(1:2)]] <- mag[freq[-(1:2)]]*2
  } else{
    # odd samples
    freq <- 0:((n-1)/2)
    mag <- fftx[freq+1]/n
    # Scale to return magnitude in time domain
    mag[freq[-1]+1] <- mag[freq[-1]+1]*2
  }
  # compute angle
  ang <- Arg(mag)
  mag <- Mod(mag)
  list(mag=mag, ang=ang, freq=freq)
}

#' @export
freqMag.matrix <- function(fftx){
  mn <- dim(fftx)
  if((mn[2]%%2)==0){
    # even samples
    freq <- 0:(mn[2]/2)
    mag <- fftx[1:mn[1], freq+1]/mn[2]
    # Scale to return magnitude in time domain
    mag[, freq[-(1:2)]] <- mag[, freq[-(1:2)]]*2
  } else{
    # odd samples
    freq <- 0:((mn[2]-1)/2)
    mag <- fftx[1:mn[1], freq+1]/mn[2]
    # Scale to return magnitude in time domain
    mag[, freq[-1]+1] <- mag[, freq[-1]+1]*2
  }
  # compute angle
  ang <- Arg(mag)
  mag <- Mod(mag)
  list(mag=mag, ang=ang, freq=freq)
}
