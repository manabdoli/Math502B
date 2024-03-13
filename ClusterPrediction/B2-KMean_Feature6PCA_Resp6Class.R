#' Cluster Model
#'
#' Full XY, PCA Feature:
#'
library(Math502B)

xyFeatures <- matrix(scan('PosComplete-FeatursFull_data.csv',
                          what = numeric(), skip = 1, sep=','),
                     ncol=361)
xyPos <- as.matrix(read.csv('PosComplete-FeatursFull_pos.csv'))

pcaRotation <- as.matrix(read.csv('PosComplete-Featurs6PCA_rotation.csv'))

xyFeatures <- x2pca(xyFeatures, pcaRotation)

## 6 Clusters from 6 PCAs
xyKmCls <- as.matrix(read.csv('PosComplete-Featurs6PCA_Kmean6.csv'))
xyKmClsCenters <- as.matrix(read.csv('PosComplete-Featurs6PCA_Kmean6Centers.csv'))

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
