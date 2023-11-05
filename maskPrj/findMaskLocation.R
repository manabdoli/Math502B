# Find location ####

# Load data
if(!exists('maskData')) load('MaskData.rda')
if(!exists('mask2m')) load('mask2m.rda')
source('rMaskGen.R')

# pick a mask randomly
k <- nrow(maskData$x)
j <- sample.int(k, 1)
y <- maskData$y[j]
ij <- k2ij(nrow(mask2m), y)
tMj <- mask2m[ij[1], ij[2], ]

## Brute force ####
rmask <- maskData$x[k, ]
rmask <- rmask/sum(rmask^2)

nx <- dim(mask2m)[1]
ny <- dim(mask2m)[2]

dprd <- matrix(0, nrow = nx, ncol = ny)
for(i in 1:nx)
  for(j in 1:ny)
    dprd[i, j] <- sum(rmask*mask2m[i,j,]/
                        sqrt(sum(mask2m[i,j,]^2)))
k2ij(nx, which.max(dprd))
ij
