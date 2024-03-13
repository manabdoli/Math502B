#' Turning a matrix to an image
#' @param x the matrix with numerical values to be used as colors
#' @param scale if TRUE, the values are scaled to 0 to 255
#' @param ... additional parameters sent to `image()`.
#' @return returns the scaled matrix and plots the image.
#' @export
mat2img <- function(x, scale=TRUE, ...){
  dots <- list(...)
  z <- x
  n <- 1:dim(z)[1]
  m <- 1:dim(z)[2]
  if(length(dim(z)[3])>0){
    scale01 <- function(x) (x-min(x))/(max(x)-min(x))
    col <- scale01(z)
    z <- matrix(1, nrow = length(n), ncol = length(m))
    if(length(intersect(c('axes', 'xlab', 'ylab', 'xtxt', 'ytxt'), names(dots)))==0){
      xlab = ''
      ylab = ''
      axes = FALSE
      image(n, m, z, xlab=xlab, ylab=ylab, axes=axes, col=col, ...)
    } else{
      image(n, m, z, col=col, ...)
    }
  } else{
    if(scale){
      mm = min(z); MM=max(z); RR=MM-mm;
      z=((z-mm)/RR-.5)*255+255/2;
    }
    if(length(intersect(c('axes', 'xlab', 'ylab', 'xtxt', 'ytxt'), names(dots)))==0){
      xlab = ''
      ylab = ''
      axes = FALSE
      image(n, m, z, xlab=xlab, ylab=ylab, axes=axes, ...)
    } else{
      image(n, m, z, ...)
    }
  }
  invisible(z)
}
