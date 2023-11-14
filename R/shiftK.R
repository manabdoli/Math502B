#' Shift arrays by K element
#'
#'
#'
#' @export

shiftK <- function(x, K, circular=TRUE, fill=0){
  n <- length(x)
  K <- -sign(K)*(abs(K) %% n)
  if(K==0) x else
    if(K>0) {
      if(circular) x[c((1+K):n, 1:K)] else
        c(x[c((1+K):n)], rep(fill, K))
    } else
      if(K<0){
        if(circular) x[c((n+K+1):n, 1:(n+K))] else
          c(rep(fill, abs(K)), x[1:(n+K)])
      }
}
