#' Turning a matrix to an image
#' @param x the matrix with numerical values to be used as colors
#' @param scale if TRUE, the values are scaled to 0 to 255
#' @param ... additional parameters sent to `image()`.
#' @return returns the scaled matrix and plots the image.
#' @export
mat2img <- function(x, rgbFilter=c(1,1,1), add= FALSE,
                    xleft=0, ybottom=0, xright=1, ytop=1, ...){
  curdef <- par(mar=c(0,0,0,0)+1, no.readonly = TRUE)
  on.exit(par(curdef))
  if(!add)
    plot(0,0, type='n', xlab = '', ylab = '', axes = FALSE,
         xlim=c(xleft, xright), ylim=c(ybottom, ytop))
  if(any(rgbFilter!=1)){
    xwide <- dim(x)
    xlong <- c(prod(xwide[1:2]), xwide[3])
    x <- `dim<-`(
      `dim<-`(x, xlong)*(matrix(1, nrow = xlong[1])%*%rgbFilter), xwide)
  }
  x <- (x-min(x))/(max(x)-min(x))
  rasterImage(as.raster(x), xleft, ybottom, xright, ytop)
  invisible(x)
}
