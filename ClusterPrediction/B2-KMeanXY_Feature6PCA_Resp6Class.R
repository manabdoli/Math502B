#' Cluster Model
#'
#' Full XY, PCA Feature + XY:
#'
library(Math502B)

xyFeatures <- matrix(scan('PosComplete-FeatursFull_data.csv',
                          what = numeric(), skip = 1, sep=','),
                     ncol=361)
xyPos <- as.matrix(read.csv('PosComplete-FeatursFull_pos.csv'))

pcaRotation <- as.matrix(read.csv('PosComplete-Featurs6PCA_rotation.csv'))

xyFeatures <- x2pca(xyFeatures, pcaRotation)

## 6 Clusters from 6 PCAs
xyKmCls <- as.matrix(read.csv('PosComplete-Featurs6PCA_KmeanXY6.csv'))
xyKmClsCenters <- as.matrix(read.csv('PosComplete-Featurs6PCA_KmeanXY6Centers.csv'))

kmCls <- sort(unique(xyKmCls))
kmClsK <- length(kmCls)

xyKmClsCenterPos <- clusterCenter(xyPos, xyKmCls)

ClusCols <- hsv(h = (1:kmClsK)/kmClsK, s = 1, v = .8, alpha = .5)
plot(xyPos, pch=16, cex=.5, col=ClusCols[xyKmCls])
points(xyKmClsCenterPos, pch=as.character(1:kmClsK), cex=2, col='black')

# 10-fold train-test sets
# A generic Cluster Model based on given Clusters
xyPCAklsModle <- clusterModel(x = xyFeatures, y = xyKmCls, center = xyKmClsCenters)

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
