library(jpeg)
x1 <- readJPEG(source = 'PCA/img/SandDunes.jpeg')
plot(1:2, type='n', ann = FALSE, axes = FALSE)
rasterImage(x1, 1, 1, 2, 2)

library(imager)
x1c <- load.image(file = 'PCA/img/SandDunes.jpeg')
x1g <- grayscale(x1c)
rasterImage(x1g, 1, 1, 2, 2)

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

x1g2d <- x1g[, , 1, 1]
pca1 <- prcomp(x1g2d)
pca_spree(pca1, xlim=c(0,15))
nPC <- 3
y1 <- pca_reconstr(pca1, nPC)
range(y1)
y1[y1>1] <- 1
plot(1:2, type='n', ann = FALSE, axes = FALSE)
y1img <- x1g
y1img[,,1,1] <- y1
rasterImage(y1img, 1, 1, 2, 2)
