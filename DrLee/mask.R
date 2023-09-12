library(R.matlab)
x <- readMat(file.choose())
str(x)
xdim <- dim(x$mask2m)
plot(x$mask2m[1, 2, ], type='l')
lines(x$mask2m[1, 5, ])
rx <- sample.int(xdim[1], 1)
ry <- sample.int(xdim[2], 1)
rmask <- x$mask2m[rx, ry,]
rmask <- rmask/sum(rmask^2)

dprd <- matrix(0, nrow = rx, ncol = ry)
for(i in 1:rx)
  for(j in 1:ry)
    dprd[i, j] <- sum(rmask*x$mask2m[i,j,]/
                        sum(x$mask2m[i,j,]^2))
which.max(dprd)
