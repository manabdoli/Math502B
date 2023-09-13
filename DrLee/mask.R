library(R.matlab)
k2ij <- function(nx, k){
  c((k-1)%%nx+1, (k-1)%/%nx+1)
}

if(file.exists('mask2m.rda')){
  load('mask2m.rda')
} else{
  x <- readMat(file.choose())
  mask2m <- x$mask2m
  save(mask2m, file = 'mask2m.rda')
}

str(mask2m)
xdim <- dim(mask2m)
plot(mask2m[1, 2, ], type='l')
lines(mask2m[1, 5, ])
rx <- sample.int(xdim[1], 1)
ry <- sample.int(xdim[2], 1)
rmask <- mask2m[rx, ry,]
rmask <- rmask/sum(rmask^2)

dprd <- matrix(0, nrow = rx, ncol = ry)
for(i in 1:rx)
  for(j in 1:ry)
    dprd[i, j] <- sum(rmask*mask2m[i,j,]/
                        sqrt(sum(mask2m[i,j,]^2)))
k2ij(rx, which.max(dprd))
c(rx, ry)

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
