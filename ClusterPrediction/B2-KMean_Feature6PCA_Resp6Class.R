#' Cluster Model
#'
#' Full XY, PCA Feature:
#'
library(Math502B)

fpath <- function(file, folder='ClusterPrediction') paste0(folder, '/', file)
xyFeatures <- matrix(scan(fpath('PosComplete-FeatursFull_data.csv'),
                          what = numeric(), skip = 1, sep=','),
                     ncol=361)

xyPos <- as.matrix(read.csv(fpath('PosComplete-FeatursFull_pos.csv')))

lul <- range(xyFeatures)
k <- 10
cutls <- seq(lul[1], lul[2], length=k+1)
cutls[1] <- cutls[1]-.1*diff(cutls[1:2])
cutls[k+1] <- cutls[k+1] + .1*diff(cutls[1:2])


#z <- apply(xyFeatures, 1, \(v) diff(range(v)))
z <- do.call('rbind', apply(xyFeatures, 1, \(v) (table(cut(v, cutls))), simplify = F))
dim(z) <- c(304,304,k)
#
i=3; mat2img(z[,,i+(1:3)])

#
str(xyPos)

pcaRotation <- as.matrix(read.csv(fpath('PosComplete-Featurs6PCA_rotation.csv')))

xyFeatures <- x2pca(xyFeatures, pcaRotation)

#
mat2img(`dim<-`(scale(xyFeatures[,1:3], F), c(304, 304, 3)))

#

## 6 Clusters from 6 PCAs
xyKmCls <- as.matrix(read.csv(fpath('PosComplete-Featurs6PCA_Kmean6.csv')))
xyKmClsCenters <- as.matrix(read.csv(fpath('PosComplete-Featurs6PCA_Kmean6Centers.csv')))

kmCls <- sort(unique(xyKmCls))
kmClsK <- length(kmCls)

xyKmClsCenterPos <- clusterCenter(xyPos, xyKmCls)

ClusCols <- hsv(h = (1:kmClsK)/kmClsK, s = 1, v = .8, alpha = .5)
plot(xyPos, pch=16, cex=.5, col=ClusCols[xyKmCls])
points(xyKmClsCenterPos, pch=as.character(1:kmClsK), cex=2, col='black')

# 10-fold train-test sets
# A generic Cluster Model based on given Clusters
PCAklsModle <- clusterModel(x = xyFeatures, y = xyKmCls, center = xyKmClsCenters)

PCAbrfResult1 <- kfold_test(x = xyFeatures, y = as.vector(xyKmCls), k = 10,
                            model = PCAklsModle, summarize = TRUE)
PCAbrfResult1dtl <- kfold_test(x = xyFeatures, y = xyKmCls, k = 10,
                               model = PCAklsModle, summarize = FALSE)

with(PCAbrfResult1dtl,
     cbind(aggregate(y!=yhat, by=list(fold), FUN=mean),
           aggregate((y-yhat)^2, by=list(fold), FUN=mean))
)

PCAbrfResult1

with(PCAbrfResult1dtl, boxplot((y-yhat)^2~fold))
