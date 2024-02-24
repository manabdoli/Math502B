#' Cluster Model
#'
#' Full XY, Full Feature:
#'
library(Math502B)

xyFeatures <- matrix(scan('PosComplete-FeatursFull_data.csv',
                          what = numeric(), skip = 1, sep=','),
                     ncol=361)
xyPos <- as.matrix(read.csv('PosComplete-FeatursFull_pos.csv'))

xyKmCls <- as.matrix(read.csv('PosComplete-FeatursFull_KmeanXY6.csv'))
xyKmClsCenters <- as.matrix(read.csv('PosComplete-FeatursFull_KmeanXY6Centers.csv'))

kmCls <- sort(unique(xyKmCls))
kmClsK <- length(kmCls)

xyKmClsCenterPos <- lapply(1:kmClsK,
                           \(j) cbind(median(xyPos[xyKmCls==j, 1]),
                                      median(xyPos[xyKmCls==j, 2]))
) |> do.call(what='rbind')

ClusCols <- hsv(h = (1:kmClsK)/kmClsK, s = 1, v = .8, alpha = .5)
plot(xyPos, pch=16, cex=.5, col=ClusCols[xyKmCls])
points(xyKmClsCenterPos, pch=as.character(1:kmClsK), cex=2, col='black')

# 10-fold train-test sets
kFold <- 10
TrTsIdx <- split(data.frame(i=sample(dim(xyFeatures)[1])), (1:dim(xyFeatures)[1]) %% kFold)

MCRs <- MSEs <- posMSEs <- rep(0, kFold)
for(m in 1:kFold){
  trData <- xyFeatures[-TrTsIdx[[m]][[1]], ]
  trResp <- xyKmCls[-TrTsIdx[[m]][[1]], ]
  trPos <- xyPos[-TrTsIdx[[m]][[1]], ]

  tsData <- xyFeatures[TrTsIdx[[m]][[1]], ]
  tsResp <- xyKmCls[TrTsIdx[[m]][[1]], ]
  tsPos <- xyPos[TrTsIdx[[m]][[1]], ]


  kmXY6Model <- clusterModel(x = trData, y = trResp, center = xyKmClsCenters)

  prdDist <- predict(kmXY6Model, newdata = tsData, type = 'dist')
  prdClass <- predict(kmXY6Model, newdata = tsData, type = 'resp')
  MCRs[m] <- mean(prdClass!=tsResp)
  MSEs[m] <- mean((tsData-xyKmClsCenters[prdClass, ])^2)
  posMSEs[m] <- mean((tsPos-xyKmClsCenterPos[prdClass,])^2)
  cat('Fold ', m, ': \tMCR=', MCRs[m], '\tMSE=', MSEs[m], '\tPosMSE=', posMSEs[m], '\n')
}
