#' Cluster Class: A Generic Distance Based Classification Model
#' @param x training data, a numerical matrix
#' @param y classification for training data
#' @param center if `mean` (default), uses the mean of each cluster as its center.
#'  if a numerical matrix, it is used as centers.
#' @param dist if `euclidean` (default), the euclidean distance is used to
#'  compute the distance from the center in prediction step; otherwise, this is
#'  a function to compute the distance in prediction step.
#' @export
clusterModel <- function(x, y, center='mean', dist='euclidean'){
  # builds a Distance Based model where clusters in `y` are represented
  # with `center`, or calculated if `center` is not a numerical matrix

  # Turn x into a matrix if needed
  if(!is.matrix(x)) x <- matrix(x, ncol=1)

  # Number of classes
  cLevels <- if(is.factor(y)) levels(y) else sort(unique(y))
  k <- length(cLevels)

  # colMeans to compute center by means
  colMeans <- function(x){
    colSums(x)/dim(x)[1]
  }
  # checking centers
  if(is.numeric(center)){
    # centers are given
    if(dim(center)[2]!=dim(x)[2])
      stop('Dimension of Centers should match those of `x`!')
    if(dim(center)[1]<k)
      stop('Insufficeint number of centers are given')

  } else{
    # if numerical centers are not given
    if(center == 'mean'){
      center <-  lapply(1:k, \(j) colMeans(x[y==cLevels[j],,drop=FALSE]))
      center <- do.call('rbind', center)
    } else{
      stop(sprintf('%s is not a defined method for finding centers!'))
    }
  }
  # Distance function
  dist2center <- if(is.function(dist)) dist else{
    if(dist=='euclidean'){
      function(x){
        sapply(1:k, \(i)
               tryCatch(sqrt(sum((center[i,]-x)^2)),
                        error=\(e) {
                          warning('Error in calculating distance:', e)
                          NaN
                        })
        )
      }
    } else{
        stop('Distance method is not valid.')
      }
  }

  # Model
  structure(
    list(x=x, # Training data
         y=y, # Cluster Responses
         center = center, # cluster representatives,
         k = k, # number of distinct clusters
         classes=cLevels, # list of clusters
         dist2center = dist2center
    ),
    class = c('ClusterClass')
  )
}

#' @rdname clusterModel
#' @description
#' Prediction function for ClusterClass models
#' @param object a ClusterClass object
#' @param newdata test data to be used for prediction using the object model
#' @param type if `response` (default), the predicted classes are retrurned.
#'  If `distance` is used, the distance of each row in newdata to all clusters
#'   is returned
#' @export
predict.ClusterClass <- function(object, newdata,
                                 type = 'response'){
  # type
  type <- match.arg(type, c('response', 'distance'))

  # fixing the newdata form
  if(!is.matrix(newdata)) newdata <- matrix(newdata, ncol=1)

  # compute distance to each cluster
  distances <- t(apply(newdata, 1, object$dist2center))

  # return distance if needed
  if(type=='distance') return(distances)

  # find closest cluster
  response <- apply(distances, 1, \(x) which.min(x)[1])
  response
}

# Print overload
print.ClusterClass <- function(x, ...){
  str(x)
}
