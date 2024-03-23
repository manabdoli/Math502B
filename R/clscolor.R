#' Cluster Colors: a range of colors for clusters or values
#'
#' @return a function that returns a color for each number between 1 to `n`, used
#' as the hue in the HSV color scheme.
#'
#' @param n is the number of distinct colors to be produced for the data
#' @param s the saturation of HSV color with a default value of 1.
#' @param v the value of HSV color with a default value of 0.8.
#' @param alpha the alpha transparency value, with a default value of 0.5.
#' @export
clscolor <- function(n, s=1, v=0.8, alpha=0.5) {
  hsv(h = (1:n)/n, s = s, v = v, alpha = alpha)
}


#' @rdname clscolor
#' @description
#' A function to produce color for a range of values between two colors
#' @param x the vector of values, or a range of values.
#' @param n the number of distinct colors to be generated.
#' @param lowColor the color for the lowest value in the range of x
#' @param highColor the color for the highest value in the range of x
#' @param alpha the alpha transparency value for the colors, with a default
#' value of 1.
#' @return a function that returns a color for a numerical value.
#' @export
valcolor <- function(x, n){
  rng <- range(x)
  brk <- seq(0, 1+1/n, len=n+1)-.5/(n+1)
  lvl <- cut(seq(0, 1, len=n), breaks = brk) |> factor() |> levels()
  function(v, lowColor='blue', highColor='red', alpha=1){
    rngCol <- colorRampPalette(c(lowColor, highColor))(n)
    sclv <- (v-rng[1])/diff(rng)
    rngCol[cut(sclv, breaks=brk) |> factor(levels=lvl) |> as.integer()]
  }
}
