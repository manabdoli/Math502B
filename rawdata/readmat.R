library(R.matlab)
x <- readMat(file.choose())
mask2m <- x$mask2m
save(mask2m, file = 'mask2m.rda')
