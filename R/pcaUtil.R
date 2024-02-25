#' PCA utility functions
#'
#' @export
pca_spree <- function(x, ...){
  with(x, barplot(sdev^2/(sdev[1]^2), ylim=c(0, 1.1), 1, 0, border = NA, ...))
  x$cumVar <- cumsum(x$sdev^2)/sum(x$sdev^2)
  with(x, lines(1:length(sdev)-.5, cumVar, type='b'))
  invisible(x)
}

#' @export
pca_reconstr <- function(x, nPC=1){
  y <- x$x[, 1:nPC] %*% t(x$rotation[, 1:nPC])
  if(all(x$scale != FALSE)){
    y <- scale(y, center = FALSE , scale=1/x$scale)
  }
  if(all(x$center != FALSE)){
    y <- scale(y, center = -1 * x$center, scale=FALSE)
  }
  y
}


#'
#' @export
x2pca <- function(x, rotation, nPC=dim(rotation)[2], center=TRUE, scale=FALSE){
  x <- as.matrix(x)
  x <- scale(x, center = center, scale = scale)
  x %*% rotation[, 1:nPC]
}
