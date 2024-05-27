#' Cluster Model
#'
#' Full XY, PCA Feature + XY:
#'
library(Math502B)
fpath <- function(file, folder='ClusterPrediction') paste0(folder, '/', file)
data0 <- scan(fpath('PosComplete-FeatursFull_data.csv'),
                          what = numeric(), skip = 1, sep=',')
data0 <- t(`dim<-`(data0, c(361, 304*304)))
#plot(data0[1,])
xyPos <- as.matrix(read.csv(fpath('PosComplete-FeatursFull_pos.csv')))

xyFFT <- t(apply(data0, 1, fft))
xyFFTmaf <- freqMag.matrix(xyFFT)
rm(data0, xyFFT)
# Significant Frequencies based on Energy
Espec <- xyFFTmaf$mag^2
sFreq <- cbind(0, t(apply(Espec[,-1], 1, order, decreasing = TRUE)))
# Scaling and Computing Cumulative Sum
sEspec <- Espec
nil <- sapply(1:dim(sFreq)[1], \(k) sEspec[k,] <<-
                cumsum(prop.table(Espec[k,sFreq[k,]+1])));
# Plotting the first three significant frequencies
mat2img(`dim<-`(scale(sFreq[,2:4], F), c(304, 304, 3)))
mat2img(`dim<-`(log(scale(sFreq[,2:4], F)), c(304, 304, 3)))

sigCols <- apply(sEspec<.95, 1, \(r){
  ltz <- which(r)
  if(length(ltz)>0) max(ltz) else NULL
}) |> do.call(what='c')
unique(sigCols)

sigFreq <- unique(as.vector(sFreq[,1:9])) |> sort()

write.csv(file = 'SigFrqs95zlim.csv', sigFreq)

bEspec <- Espec[, sigFreq+1]
ubEspec <- apply(bEspec, 1, max)
hist(ubEspec, breaks = 100,
     main = 'Energy Distribution of \nFrequencies for top 95% Energy',
     xlab='Energy', xlim=c(0, 1000))

Elim <- 10
hist(ubEspec[ubEspec<Elim], breaks = 10, col = 'red', add=TRUE)



sigFreq <- (read.csv(file = "SigFrqs95zlim.csv", header = FALSE))[[1]]
xyFeatures <- (xyFFTmaf$mag[, sigFreq+1]>Elim)*1L

mat2img(`dim<-`(xyFeatures, c(304, 304, 33))[,,2:4])

## 6 Clusters from 6 PCAs
xyKmCls <- as.matrix(read.csv(fpath('PosComplete-Featurs6PCA_KmeanXY6.csv')))
xyKmClsCenters <- as.matrix(read.csv(fpath('PosComplete-Featurs6PCA_KmeanXY6Centers.csv')))

kmCls <- sort(unique(xyKmCls))
kmClsK <- length(kmCls)

xyKmClsCenterPos <- clusterCenter(xyPos, xyKmCls)

ClusCols <- hsv(h = (1:kmClsK)/kmClsK, s = 1, v = .8, alpha = .5)
plot(xyPos, pch=16, cex=.5, col=ClusCols[xyKmCls])
points(xyKmClsCenterPos, pch=as.character(1:kmClsK), cex=2, col='black')

# 10-fold train-test sets
# A generic Cluster Model based on given Clusters
xyPCAklsModle <- clusterModel(x = xyFeatures, y = xyKmCls)

xyPCAbrfResult1 <- kfold_test(x = xyFeatures, y = as.vector(xyKmCls), k = 10,
                              model = xyPCAklsModle, summarize = TRUE)
xyPCAbrfResult1dtl <- kfold_test(x = xyFeatures, y = xyKmCls, k = 10,
                                 model = xyPCAklsModle, summarize = FALSE)

with(xyPCAbrfResult1dtl,
     cbind(aggregate(y!=yhat, by=list(fold), FUN=mean),
           aggregate((y-yhat)^2, by=list(fold), FUN=mean))
)

xyPCAbrfResult1

with(xyPCAbrfResult1dtl, boxplot((y-yhat)^2~fold))
