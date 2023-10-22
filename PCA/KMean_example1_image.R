library(jpeg)
x1 <- readJPEG(source = 'PCA/img/SandDunes.jpeg')
plot(1:2, type='n', ann = FALSE, axes = FALSE)
rasterImage(x1, 1, 1, 2, 2)

library(imager)
x1c <- load.image(file = 'PCA/img/SandDunes.jpeg')
# Flatten: use Kmean, Reconstruct and plot
x1c.lin <- cbind(x=rep(x = 1:442, times=388),
                 y=rep(1:388, each=442))
x1c.lin <- cbind(x1c.lin, r=0, g=0, b=0)
for(i in 1:nrow(x1c)){
  x1c.lin[i, 3:5] <- x1c[x1c.lin[i,1],x1c.lin[i,2],1,1:3]
}



# PCA
pca_spree <- function(x, ...){
  with(x, barplot(sdev^2/(sdev[1]^2), ylim=c(0, 1.1), 1, 0, border = NA, ...))
  with(x, lines(1:length(sdev)-.5, cumsum(sdev^2)/sum(sdev^2), type='b'))
}

pca_reconstr <- function(x, nPC=1){
  y <- x$x[, 1:nPC] %*% t(x$rotation[, 1:nPC])
  if(all(x$scale != FALSE)){
    y <- scale(y, center = FALSE , scale=1/x$scale)
  }
  if(all(x$center != FALSE)){
    y <- scale(y, center = -1 * x$center, scale=FALSE)
  }
  y
}

# KMean
n <- dim(x1c.lin)[1]
SST <- sum(diag(cov(x1c.lin)*(n-1)))
WSS <- sapply(1:15, function(k) kmeans(x1c.lin, k)$tot.within)
plot(WSS/SST, type='b')

optKmean <- kmeans(x1c.lin, 4)
x1c.k <- `dim<-`(optKmean$cluster, c(442, 388))
x1c.col <- x1c
for(i in 1:nrow(x1c.k))
  for(j in 1:ncol(x1c.k)){
    x1c.col[i, j, 1, 1:3] <- col2rgb(x1c.k[i,j])[1:3]
}

plot(1:2, type='n', ann = FALSE, axes = FALSE)
rasterImage(x1c.col, 1, 1, 2, 2)

# ????


pca1 <- prcomp(x1c2d)
pca_spree(pca1, xlim=c(0,15))
nPC <- 3
y1 <- pca_reconstr(pca1, 3)
range(y1)
y1[y1>1] <- 1
plot(1:2, type='n', ann = FALSE, axes = FALSE)
y1img <- x1g
y1img[,,1,1] <- y1
rasterImage(y1img, 1, 1, 2, 2)
