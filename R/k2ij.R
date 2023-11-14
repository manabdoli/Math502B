#' Vector k to Matrix (i,j)
#'
#' @export
k2ij <- function(nx, k){
  cbind((k-1)%%nx+1, (k-1)%/%nx+1)
}

