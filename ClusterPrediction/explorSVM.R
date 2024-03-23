
library(Math502B)
fpath <- function(file, folder='ClusterPrediction') paste0(folder, '/', file)
data0 <- scan(fpath('PosComplete-FeatursFull_data.csv'),
              what = numeric(), skip = 1, sep=',')
data0 <- t(`dim<-`(data0, c(361, 304*304)))
#plot(data0[1,])
xyPos <- as.matrix(read.csv(fpath('PosComplete-FeatursFull_pos.csv')))

i <- sort(sample.int(n = 304, size = 30, replace = FALSE))
j <- sort(sample.int(n = 304, size = 30, replace = FALSE))
ij <- expand.grid(i, j)
idx <- ij2k(ij[,1], ij[,2], 304)
rdata <- data0[idx, ]
rdist <- dist(rdata)
rcls_hc <- hclust(rdist)
rcls_hc
plot(rcls_hc)

k <- 10
rcls_hc_k <- cutree(rcls_hc, k=k)
plot(ij, xlim=c(1,304), ylim=c(1,304), col=rcls_hc_k)

rcls_svm <- e1071::svm(rdata, rcls_hc_k)
plot(rcls_svm)
prd <- predict(rcls_svm, data0)
g <- valcolor(x = prd, n = k)
hist(x = round(prd, 0), seq(-.5, k+0.5, len=k), col=g(0:k, 'orange', 'green'))
IJ <- expand.grid(1:304, 1:304)
K <- ij2k(IJ[,1], IJ[,2], 304)
plot(IJ[, 1], IJ[, 2], col=g(prd), cex=.5)
