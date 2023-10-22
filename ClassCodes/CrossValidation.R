# Chaper 5 Lab: Cross-Validation and the Bootstrap

# The Validation Set Approach

library(ISLR)
set.seed(1)
train=sample(392,196)
train=sample(dim(Auto)[1],196)

lm.fit=lm(mpg~horsepower,data=Auto,subset=train)
attach(Auto)

# estimating MSE for the train
c(TrainMSE=mean((mpg-predict(lm.fit,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit,Auto))[-train]^2)
)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
c(TrainMSE=mean((mpg-predict(lm.fit2,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit2,Auto))[-train]^2)
)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
c(TrainMSE=mean((mpg-predict(lm.fit3,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit3,Auto))[-train]^2)
)

set.seed(2)
train=sample(392,196)
lm.fit=lm(mpg~horsepower,subset=train)
c(TrainMSE=mean((mpg-predict(lm.fit,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit,Auto))[-train]^2)
)

lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
c(TrainMSE=mean((mpg-predict(lm.fit2,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit2,Auto))[-train]^2)
)

lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
c(TrainMSE=mean((mpg-predict(lm.fit3,Auto))[train]^2),
  TestMSE=mean((mpg-predict(lm.fit3,Auto))[-train]^2)
)

# Validation Set

B=1000
TrMSE=rep(0, B)
TsMSE=rep(0, B)
TrMSE2=rep(0, B)
TsMSE2=rep(0, B)
TrMSE3=rep(0, B)
TsMSE3=rep(0, B)
for(i in 1:B){
  train=sample(392,196)
  lm.fit=lm(mpg~horsepower,subset=train)
  TrMSE[i] <- mean((mpg-predict(lm.fit,Auto))[train]^2)
  TsMSE[i] <- mean((mpg-predict(lm.fit,Auto))[-train]^2)


  lm.fit2=lm(mpg~poly(horsepower,2),data=Auto,subset=train)
  TrMSE2[i] <- mean((mpg-predict(lm.fit2,Auto))[train]^2)
  TsMSE2[i] <- mean((mpg-predict(lm.fit2,Auto))[-train]^2)

  lm.fit3=lm(mpg~poly(horsepower,3),data=Auto,subset=train)
  TrMSE3[i] <- mean((mpg-predict(lm.fit3,Auto))[train]^2)
  TsMSE3[i] <- mean((mpg-predict(lm.fit3,Auto))[-train]^2)
}

plot(TrMSE, TsMSE)
plot(TrMSE2, TsMSE2)
plot(TrMSE3, TsMSE3)

mseMat <- data.frame(Model=c(rep('Linear', B),
                             rep('Poly2', B),
                             rep('Poly3', B)),
                     TrMSE=c(TrMSE, TrMSE2, TrMSE3),
                     TsMSE=c(TsMSE, TsMSE2, TsMSE3))

boxplot(TrMSE~Model,mseMat)
boxplot(TsMSE~Model,mseMat)

# Leave-One-Out Cross-Validation
glm.fit=glm(mpg~horsepower,data=Auto)
coef(glm.fit)
lm.fit=lm(mpg~horsepower,data=Auto)
coef(lm.fit)

library(boot)
glm.fit=glm(mpg~horsepower,data=Auto)
cv.err=cv.glm(Auto,glm.fit)
cv.err$delta

cv.error=rep(0,5)
for (i in 1:5){
 glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
 cv.error[i]=cv.glm(Auto,glm.fit)$delta[1]
 }
cv.error

# k-Fold Cross-Validation

set.seed(17)
cv10 <- NULL
for(k in 1:100){
  cv.error.10=rep(0,10)
  for (i in 1:10){
    glm.fit=glm(mpg~poly(horsepower,i),data=Auto)
    cv.error.10[i]=cv.glm(Auto,glm.fit,K=10)$delta[1]
  }
  cv.error.10
  cv10 <- c(cv10, cv.error.10)
}
df <- data.frame(Error=cv10, PolyDeg=rep(1:10, 100))
boxplot(Error~PolyDeg, data=df)



# The Bootstrap

alpha.fn=function(data,index){
 X=data$X[index]
 Y=data$Y[index]
 return((var(Y)-cov(X,Y))/(var(X)+var(Y)-2*cov(X,Y)))
 }
alpha.fn(Portfolio,1:100)

set.seed(1)
alpha.fn(Portfolio,sample(100,100,replace=T))
(bCB <- boot(Portfolio,alpha.fn,R=1000))

# Estimating the Accuracy of a Linear Regression Model

boot.fn=function(data,index)
 return(coef(lm(mpg~horsepower,data=data,subset=index)))
boot.fn(Auto,1:392)
set.seed(1)
boot.fn(Auto,sample(392,392,replace=T))
boot.fn(Auto,sample(392,392,replace=T))
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower,data=Auto))$coef
boot.fn=function(data,index)
 coefficients(lm(mpg~horsepower+I(horsepower^2),data=data,subset=index))
set.seed(1)
boot(Auto,boot.fn,1000)
summary(lm(mpg~horsepower+I(horsepower^2),data=Auto))$coef


