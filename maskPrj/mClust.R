# f <- file.choose()
# # "C:\\SharedDocs\\GitHub\\Math502B\\maskPrj\\MaskData.rda"
# # "C:\\SharedDocs\\GitHub\\Math502B\\maskPrj\\mask2m.rda"
# load(f)
#
x <- mask2m
dim(x) <- c(prod(dim(x)[1:2]), 361)
# Test
#any(mask2m[1,1, ]!=x[1,])
#any(mask2m[1,2, ]!=x[305,])

# # Kmean Clustring
kMC <- kmeans(x = x, centers = 6)
# MClust: takes awefully long
library(mclust)
#mcMC <- mclust::Mclust(x)

maskClass <- list(x=x, kMean=kMC$cluster, MC=mcMC$classification)

#save(x, kMC, mcMC, maskClass, file = 'maskPrj/maskClusters.rda')
write.csv(x, file = 'maskLongData.csv', row.names = FALSE)
write.csv(data.frame(kMean=kMC$cluster, MC=mcMC$classification),
          file = 'maskLongDataClass.csv', row.names = FALSE)


load('maskPrj/maskClusters.rda')




plot(mcMC, what = 'BIC')
plot(mcMC, what = 'density', dimens = c(1,200))
plot(mcMC, what = 'uncertainity')


plot(x = rep(1:304, times=304), rep(1:304, each=304), pch=16, cex=.5, col=adjustcolor(kMC$cluster, .5))


plot(x = rep(1:304, times=304), rep(1:304, each=304), pch=16, cex=.5, col=adjustcolor(mcMC$classification, .5))
#range(x[,1])



rsq <- rep(0, 10)
for(k in 1:10){
  rsq[k] <- with(kmeans(x, k), 1-tot.withinss/totss)
}
plot(1:10, rsq)
