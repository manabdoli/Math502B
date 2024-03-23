#' Vector position index k to Matrix position index (i,j)
#' @return a bi-column matrix containing (i, j), showing the equivalent position for
#' positions in k in a linear presentation of a matrix with `nrow` rows.
#' @param nrow the number of rows in the matrix representation of data.
#' @param k the list of linear position indexes.
#' @export
k2ij <- function(nrow, k){
  cbind((k-1)%%nrow+1, (k-1)%/%nrow+1)
}

