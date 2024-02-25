#' gridClust creates cluster of a plane using grids
#'

#' Divide 1:n by k
breakNbyK <- function(n, k){
  if(k<0 || k>n){
    warning('invalid k, no breakdown in performed (k=1).')
    k <- 1
    return(rep(1, n))
  }
  # Width of each break
  breakW <- round(n/k, 0)
  idx <- rep(1:k, each=breakW)
  # Cut from both sides when needed
  d <- length((idx))-n
  if(d==0) return(idx)
  if(d>0){
    d <- round(d/2, 0)
    return(idx[d+(1:n)])
  } else{
    d <- -round(d/2, 0)
    idx <- c(rep(1, d), idx, rep(k, n-length(idx)-d))
    return(idx)
  }
}

gridClust <- function(xydim, gridn, long=FALSE){
  nx <- xydim[1]
  ny <- if(length(xydim)==2) xydim[2] else xydim[1]
  #
  gx <- gridn[1]
  gy <- if(length(gridn)==2) gridn[2] else gridn[1]
  #
  gxcls <- breakNbyK(nx, gx)
  gycls <- breakNbyK(ny, gx)

  structure(list(xcls=gxcls, ycls=gycls),
            xydim = xydim,
            gridSize = gridn,
            class=c('gridCluster', 'list')

  )
}

#' grid Indexes
`[.gridCluster` <- function(x, i, j){
  xIdx <- which(x$xcls==i)
  yIdx <- which(x$ycls==j)
  list(x=rep(xIdx, length(yIdx)),
       y=rep(yIdx, each=length(xIdx)))
}

cluster.gridCluster <- function(x, i, j){
  gridn <- attr(x, 'gridSize')
  if(is.null(gridn)) stop('Grid Size is missing.')
  rep(i, length(j))+(rep(j, each=length(i))-1)*gridn[1]
}

xygrid <- gridClust(304, 3)
xygrid[1,3]
cls <- cluster.gridCluster(xygrid, 1:3, 1:3)

plot(1:304, 1:304, pch='.', type = 'n')
for(i in 1:3){
  for(j in 1:3){
    xys <- xygrid[i, j]
    points(xys$x, xys$y, pch=(i-1)*3+j, col=(i-1)*3+j, cex=.2)
  }
}

