#' Finding the center of a cluster
#'
#' @export
clusterCenter <- function(x, cluster, method=c('median')){
  qCls <- sort(unique(cluster))
  kCls <- length(qCls)
  method <- match.arg(tolower(method), c('median', 'mean'))
  if(method=='median'){
    lapply(1:kCls,
           \(j) rbind(apply(x[cluster==qCls[j], ], 2, median))
    ) |> do.call(what='rbind')
  } else if(method=='mean'){
    lapply(1:kCls,
           \(j) rbind(apply(x[cluster==qCls[j], ], 2, mean))
    ) |> do.call(what='rbind')
  }
}
