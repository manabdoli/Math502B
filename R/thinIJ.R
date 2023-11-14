#' Thin a matrix
#'
#' @export
thinIJ <- function(x, by=2, startIJ=c(1,1), returnIJ=TRUE){
  if(length(by)==1) by <- c(by, by)
  if(length(startIJ)==1) startIJ <- c(startIJ, startIJ)
  Is <- seq(startIJ[1], dim(x)[1], by[1])
  Js <- seq(startIJ[2], dim(x)[2], by[2])
  if(returnIJ) list(Is=Is, Js=Js) else
    if(length(dim(x))==2) x[Is, Js] else
      if(length(dim(x))==3) x[Is, Js, ] else
        stop('x should be a 2 or 3 dimension matrix')
}
