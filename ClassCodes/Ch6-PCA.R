# PCA

library(ISLR)
#View(Hitters)
names(Hitters)
dim(Hitters)
Hitters=na.omit(Hitters)
dim(Hitters)



# Separating Response
x=model.matrix(Salary~.,Hitters)[,-1]
y=Hitters$Salary

pr.out=prcomp(x, scale=TRUE)
Cols=function(vec){
  cols=rainbow(length(unique(vec)))
  return(cols[as.numeric(as.factor(vec))])
}
par(mfrow=c(1,2))
plot(pr.out$x[,1:2], col=Cols(y), pch=19,xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(y), pch=19,xlab="Z1",ylab="Z3")
plot(pr.out$x[,c(1,4)], col=Cols(y), pch=19,xlab="Z1",ylab="Z4")
summary(pr.out)
plot(pr.out)
pve=100*pr.out$sdev^2/sum(pr.out$sdev^2)
par(mfrow=c(1,2))
plot(pve,  type="o", ylab="PVE", xlab="Principal Component", col="blue")
plot(cumsum(pve), type="o", ylab="Cumulative PVE", xlab="Principal Component", col="brown3")



