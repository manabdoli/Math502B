#' Parallel Processing Example
#' 
# install.packages("parallel")
# install.packages("foreach")
# install.packages("doParallel")

library(parallel)
library(foreach)
library(doParallel)

# Set the number of cores you want to use
num_cores <- detectCores()

# Register parallel backend
registerDoParallel(cores = num_cores)

# Function to perform K-means clustering
kmeans_parallel <- function(data, centers, iter.max = 10) {
  kmeans(data, centers = centers, iter.max = iter.max)
}


# Example data
set.seed(123)
data <- matrix(rnorm(1000), ncol = 2)

# Specify number of clusters
k <- 3

# Generate initial cluster centers
initial_centers <- data[sample(nrow(data), k), ]

# Perform K-means clustering in parallel
results <- foreach(i = 1:num_cores, .combine = c) %dopar% {
  
  # Subset of data to process on each core
  idx <- (i-1)*ceiling(nrow(data)/num_cores) + 1:ceiling(nrow(data)/num_cores)
  idx <- idx[which(idx<nrow(data))]
  subset_data <- data[idx, ]
  
  # Perform K-means clustering on the subset
  kmeans_parallel(subset_data, centers = initial_centers)
}

# Combine results from all cores
final_clusters <- do.call("rbind", lapply(results, function(x) x$cluster))

# Stop parallel backend
stopImplicitCluster()
