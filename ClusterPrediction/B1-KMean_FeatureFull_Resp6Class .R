#' Cluster Model
#'
#' Full XY, Full Feature:
#'
library(Math502B)

xyFeatures <- matrix(scan('PosComplete-FeatursFull_data.csv',
                          what = numeric(), skip = 1, sep=','),
                     ncol=361)
xyPos <- as.matrix(read.csv('PosComplete-FeatursFull_pos.csv'))

xyKmCls <- as.matrix(read.csv('PosComplete-FeatursFull_Kmean6.csv'))
xyKmClsCenters <- as.matrix(read.csv('PosComplete-FeatursFull_Kmean6Centers.csv'))

kmCls <- sort(unique(xyKmCls))
kmClsK <- length(kmCls)

xyKmClsCenterPos <- clusterCenter(xyPos, xyKmCls)

ClusCols <- hsv(h = (1:kmClsK)/kmClsK, s = 1, v = .8, alpha = .5)
plot(xyPos, pch=16, cex=.5, col=ClusCols[xyKmCls])
points(xyKmClsCenterPos, pch=as.character(1:kmClsK), cex=2, col='black')

# A generic Cluster Model based on given Clusters
klsModle <- clusterModel(x = xyFeatures, y = xyKmCls, center = xyKmClsCenters)

brfResult1 <- kfold_test(x = xyFeatures, y = as.vector(xyKmCls), k = 10, model = klsModle, summarize = TRUE)
brfResult1dtl <- kfold_test(x = xyFeatures, y = xyKmCls, k = 10, model = klsModle, summarize = FALSE)

with(brfResult1dtl,
     cbind(aggregate(y!=yhat, by=list(fold), FUN=mean),
           aggregate((y-yhat)^2, by=list(fold), FUN=mean))
)

brfResult1

with(brfResult1dtl, boxplot((y-yhat)^2~fold))
