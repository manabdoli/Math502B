library(R.matlab)

k2ij <- function(nx, k){
  c((k-1)%%nx+1, (k-1)%/%nx+1)
}

rMask <- function(x # True Mask
                  ,tilt_err=0.1 # Tilt error
                  ,mask_err=1
                  ,az_cover=2*pi # The azimuth coverage of mask
                  ){
  # Number of points on one mask
  nAz <- if(is.matrix(x)) dim(x)[2] else length(x)
  # Azimuth angles
  az <- seq(0, az_cover, length.out = nAz)
  tiltAngle=rnorm(1, 0, tilt_err)
  noise_tilt=cos(az-runif(1, 0, 2*pi))*tiltAngle
  noise_mask=rnorm(nAz)*mask_err

  if(is.matrix(x)){
    x+matrix(1,nrow = dim(x)[1], ncol = 1)%*%
      matrix(noise_tilt+noise_mask, nrow=1)
  } else
    x+noise_tilt+noise_mask
}

# Load True Masks
if(file.exists('mask2m.rda')){
  load('mask2m.rda')
} else{
  x <- readMat(file.choose())
  mask2m <- x$mask2m
  save(mask2m, file = 'mask2m.rda')
}

str(mask2m)
tM11 <- mask2m[1,1,];
rM11 <- rMask(tM11, tilt_err = 1, mask_err = .1, az_cover = 2*pi);
plot(tM11, cex=.5, col=adjustcolor('red', .3))
lines(rM11, col=adjustcolor('blue', .5))
points(rM11, pch='.', cex=2)

fftM11 <- fft(tM11)
ffrM11 <- fft(rM11)

plot(fftM11, pch='.', col=adjustcolor('blue', .5), cex=3)
points(ffrM11, pch='.', col=adjustcolor('red', .5), cex=3)

plot(
  apply(cbind(fftM11, ffrM11), 1, \(x) {Mod(x[1]-x[2])}),
  col=adjustcolor('orange',.5), cex=.5)

xdim <- dim(mask2m)
plot(mask2m[1, 2, ], type='l')
lines(mask2m[1, 5, ])

{
  nx <- xdim[1]
  ny <- xdim[2]
  rx <- sample.int(xdim[1], 1)
  ry <- sample.int(xdim[2], 1)
  rmask <- mask2m[rx, ry,]
  rmask <- rmask/sum(rmask^2)

  dprd <- matrix(0, nrow = nx, ncol = ny)
  for(i in 1:nx)
    for(j in 1:ny)
      dprd[i, j] <- sum(rmask*mask2m[i,j,]/
                          sqrt(sum(mask2m[i,j,]^2)))
  k2ij(nx, which.max(dprd))
  c(rx, ry)
}
# Plot
library(rgl)
x <- rep(10 * (1:nrow(dprd)), ncol(dprd)) # x coordinates
y <- rep(10 * (1:ncol(dprd)), each = nrow(dprd)) # y coordinates
z <- as.vector(dprd) # z coordinates
plot3d(x, y, z, col = heat.colors(length(z)),
       type = "s", size = 1, aspect = 1)
surface3d(x = x, y = y, z = z)

library(plotly)
plot_ly(x = ~x, y = ~y, z = ~z,
        type = "mesh3d", intensity = ~z)
plot_ly(x = ~x, y = ~y, z = ~z,
        type = "mesh3d", intensity = ~z)


mask2D <- mask2m[1,,]
for(i in 2:304){
  mask2D <- rbind(mask2D, mask2m[i,,])
}

str(mask2D)

# PCA ####
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

pcaMsk <- prcomp(mask2D)

{
  nx <- xdim[1]
  ny <- xdim[2]
  rx <- sample.int(xdim[1], 1)
  ry <- sample.int(xdim[2], 1)
  rmask <- mask2m[rx, ry,]
  rmask <- rmask/sum(rmask^2)

  dprd <- matrix(0, nrow = nx, ncol = ny)
  for(i in 1:nx)
    for(j in 1:ny)
      dprd[i, j] <- sum(rmask*mask2m[i,j,]/
                          sqrt(sum(mask2m[i,j,]^2)))
  k2ij(nx, which.max(dprd))
  c(rx, ry)
}



## Checking PCA
X <- mask2D %*% pcaMsk$rotation


{
  rmask <- mask2m[rx, ry,]
  rpcamask <- rmask %*% pcaMsk$rotation
  rpcamask <- rpcamask[1:3]

  dprd <- matrix(0, nrow = nx, ncol = ny)
  for(i in 1:dim())
    for(j in 1:ny)
      dprd[i, j] <- sum(rpcamask*X[i,j,]/
                          sqrt(sum(mask2m[i,j,]^2)))
  k2ij(nx, which.max(dprd))
  c(rx, ry)
}
