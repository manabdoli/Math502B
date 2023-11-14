#' PCA utility functions
#'
#' @export
pca_spree <- function(x, ...){
  with(x, barplot(sdev^2/(sdev[1]^2), ylim=c(0, 1.1), 1, 0, border = NA, ...))
  with(x, lines(1:length(sdev)-.5, cumsum(sdev^2)/sum(sdev^2), type='b'))
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
