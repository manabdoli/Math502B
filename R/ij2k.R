#' Matrix position index (i,j) to Vector position index k
#' @return a vector of linear position indexes equivalent to the bi-column matrix
#' (i, j), representing position indexes in a matrix with `nrow` rows.
#' @param i a vector of positions in the first dimension, or a bi-column matrix
#' containing the positions in two dimensions.
#' @param j the vector of positions in the second dimension, if `i` is not a bi-column matrix
#' @param nrow the number of rows in the matrix representation of data.
#' @export
ij2k <- function(i, j=NULL, nrow){
  if(is.matrix(i) && dim(i)[2]>1){
    j <- i[,2]
    i <- i[,1]
  }
  i+(j-1)*nrow
}
