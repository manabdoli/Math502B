# Toy problem

set.seed(123)
rho <- 0.65
n <- 100
x <- rnorm(n, c(-3, 4))
y <- rho*x + sqrt(1-rho^2)*rnorm(n)

x <- 2*x-4
y <- 3*y+5

plot(x,y, asp = 1)


X <- cbind(x,y)

# K Mean
M <- apply(X, 2, mean); M <- matrix(1, nrow = n, ncol = 1) %*% M
# K Mean
k=2
x2Means <- kmeans(X, centers = k)
plot(X, col=x2Means$cluster, asp=1)
points(x2Means$centers, pch=16, col=adjustcolor(1:2, 0.7), cex=2)

k=3
x3Means <- kmeans(X, centers = k)
plot(X, col=x3Means$cluster, asp=1)
points(x3Means$centers, pch=16, col=adjustcolor(1:2, 0.7), cex=2)

x3Means <- kmeans(X, centers = k)
plot(X, col=x3Means$cluster, asp=1)
points(x3Means$centers, pch=16, col=adjustcolor(1:2, 0.7), cex=2)

# Picking the optimum k
SST <- sum(diag(cov(X)*(n-1)))
WSS <- sapply(1:5, function(k) kmeans(X, k)$tot.within)
plot(WSS/SST, type='b')
