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
M <- apply(X, 2, mean); M <- matrix(1, nrow = n, ncol = 1) %*% M

# Finding Covariance Matrix
XtX <- t(X)%*%X
MtM <- t(M)%*%M
Sig <- (XtX - MtM)/(n-1)

range(Sig-cov(X)) # Rounding Error

# Sig's Eigen Values and Vectors: L.D.Lt
EVs <- eigen(Sig)
D <- EVs$values
L <- EVs$vectors

range(Sig-L%*%diag(D)%*%t(L))

# Variance is preserved
sum(D) - sum(diag(Sig))

# PCs using Eigen Vectors
Y <- (X-M)%*%L
plot(Y, asp=1)

# SVD: X-M = USVt
Xsvd <- svd(X-M)
U <- Xsvd$u
S <- Xsvd$d
V <- Xsvd$v
range(X-M-U%*%diag(S)%*%t(V))

# Sig = VS^2Vt/(n-1)
range(Sig-V%*%diag(S^2)%*%t(V)/(n-1))


# PRComp
Xpcs <- prcomp(X, center = TRUE, scale. = FALSE)
points(Xpcs$x, col='red')

plot(Y, asp=1)
points(Xpcs$x%*%diag(c(1,-1)), col='green', pch=3)


# SVD: (X-M)/sqrt(n-1) = USVt
Xsvd <- svd((X-M))
U <- Xsvd$u
S <- Xsvd$d
V <- Xsvd$v
## how prcomp computes rotation and PCs
SD <- S/sqrt(n-1) # rotation
SU <- (X-M) %*% V # PC's

plot(Y, asp = 1)
points(SU%*%diag(c(1, -1)), col='purple', pch=4)

# Variance
with(Xpcs, barplot(sdev^2/sum(sdev^2), ylim=c(0, 1.1), 1, 0, ))
with(Xpcs, lines(1:length(sdev)-.5, cumsum(sdev^2)/sum(sdev^2), type='b'))


# Using PC1 to classify data
with(Xpcs, plot(x[,1], y=0*x[,1]))
# Group 1: x1<0
# Group 2: x1>0
grp <- ifelse(Xpcs$x[,1]<0, 'Grp1', 'Grp2')
plot(Xpcs$x, col=factor(grp))
# We were able to use PC1 to cluster the data
plot(X, col=factor(grp))
# Predict the group of some points: c(-5, 0) and c(7, 15)
points(v1 <- cbind(-5, 0), pch=16, col='purple')
points(v2 <- cbind(7, 15), pch=16, col='purple')

plot(Xpcs$x, col=factor(grp))
z1 <- (v1-M[1,]) %*% Xpcs$rotation
points(z1, col=ifelse(z1[,1]<0, 1, 2), pch=16, xpd=T)
z2 <- (v2-M[1,]) %*% Xpcs$rotation
points(z2, col=ifelse(z2[,1]<0, 1, 2), pch=16, xpd=T)

# Reconstructing Data from PC1
y1 <- Xpcs$x[, 1] %*% t(Xpcs$rotation[, 1])
if(all(Xpcs$scale != FALSE)){
  y1 <- scale(y1, center = FALSE , scale=1/Xpcs$scale)
}
if(all(Xpcs$center != FALSE)){
  y1 <- scale(y1, center = -1 * Xpcs$center, scale=FALSE)
}

plot(y1, col=factor(grp), pch=3)
points(Xpcs$x, col=factor(grp))
points(X, col='green')
