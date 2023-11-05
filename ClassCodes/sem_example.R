#' Factor Analysis
library(psych)
# Load the iris dataset
data(iris)

# Scale the variables
iris_scaled <- scale(iris[, 1:4])

# Perform factor analysis with two factors and varimax rotation
fa_result <- fa(r = iris_scaled, nfactors = 2, rotate = "varimax")

# Print the factor loadings
print(fa_result$loadings)

# Plot the factor scores
plot(fa_result$scores, pch = 21, bg = iris$Species, xlab = "Factor 1", ylab = "Factor 2")




#' https://stats.oarc.ucla.edu/r/seminars/rsem/
#'
### DATA PREPRATION ###
library(lavaan)
dat <- read.csv("https://stats.idre.ucla.edu/wp-content/uploads/2021/02/worland5.csv")

#sample variance-covariance matrix
cov(dat)

### MODEL 1: SIMPLE REGRESSION ###

#simple regression using lm()
m1a <- lm(read ~ motiv, data=dat)
(fit1a <-summary(m1a))

### MODELS 1 AND 2: REGRESSION ###
#simple regression using lavaan
m1b <-   '
  # regressions
  # regressions
    read ~ 1 + motiv
  # variance (optional)
  motiv ~~ motiv
'
fit1b <- sem(m1b, data=dat)
summary(fit1b)

#obtain sample mean and variance
mean(dat$motiv)
var(dat$motiv)

#convert least squares to maximum likelihood
498/500*(fit1a$sigma)**2

#multiple regression
m2 <- '
  # regressions
    read ~ 1 + ppsych + motiv
    #covariance
    #ppsych ~~ motiv
'
fit2 <- sem(m2, data=dat)
summary(fit2)

### MODEL 3:  MULTIVARIATE REGRESSION ###

#multivariate regression (default)
m3a <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv
'
fit3a <- sem(m3a, data=dat)
summary(fit3a)
inspect(fit3a,"partable")
#regression of read on psych and motiv
m3b <- lm(read ~ ppsych + motiv, data=dat)
(fit3b <- summary(m3b))

#regression of arith on motiv
m3c <- lm(arith ~ motiv, data=dat)
(fit3c <- summary(m3c))

#multivariate regression (set covariance to 0)
m3d <- '
  # regressions
    read ~ ppsych + motiv
    arith ~  motiv
  # covariance
   read ~~ 0*arith
'
fit3d <- sem(m3d, data=dat)
summary(fit3d)

#known values
cov(dat[,c("read","arith","ppsych","motiv")])

#multivariate regression (saturated model)
m3e <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ ppsych + motiv
    #covariance
    ppsych ~~ motiv
    ppsych ~~ ppsych
    motiv ~~ motiv
'
fit3e <- sem(m3e, data=dat)
summary(fit3e)
#inspect(fit3e,"partable")

### MODEL 4:  PATH ANALYSIS ###

#path analysis model
m4a <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv + read
  # covariance
  #  read ~~ 0*arith
'
fit4a <- sem(m4a, data=dat)
summary(fit4a)

#re-run with fit indexes
summary(fit4a, fit.measures=TRUE)

#model chi-square
pchisq(q=4.870,df=1,lower.tail=FALSE)

#modification index
modindices(fit4a,sort=TRUE)

## model4a customized for degrees of freedom
m4aa <- '
  # regressions
    read ~ 1 + ppsych + motiv
    motiv ~ 1 + arith + read
  # covariance
  #  read ~~ 0*arith
'
fit4aa <- sem(m4aa, data=dat)
summary(fit4aa)

#path analysis model after modification
m4b <- '
  # regressions
    read ~ ppsych + motiv
    arith ~ motiv + read + ppsych
'
fit4b <- sem(m4b, data=dat)
summary(fit4b)
modindices(fit4b,sort=TRUE)

#baseline model
m4c <- '
  # variances only
    read ~~ read
    ppsych ~~ ppsych
    motiv ~~ motiv
    arith ~~ arith
'
fit4c <- sem(m4c, data=dat)
summary(fit4c, fit.measures=TRUE)

### MODEL 5:  MEASUREMENT MODEL ###

#exogenous factor analysis for adjust
m5a <- 'risk =~ verbal + ses + ppsych
          #intercepts (nu = tau)
          verbal ~ 1
          ses ~ 1
          ppsych ~ 1'
fit5a <- sem(m5a, data=dat)
summary(fit5a, standardized=TRUE)

### MODEL 6: STRUCTURAL REGRESSION ###

#structural regression (one endogenous variable)
m6a <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ppsych + ses
    achieve =~ read + arith + spell
  # regressions
    achieve ~ adjust + risk
'
fit6a <- sem(m6a, data=dat)
summary(fit6a, standardized=TRUE, fit.measures=TRUE)

#structural regression (two endogenous variables)
m6b <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
    achieve =~ read + arith + spell
  # regressions
    adjust ~ risk
    achieve ~ adjust + risk
'
fit6b <- sem(m6b, data=dat)
summary(fit6b, standardized=TRUE, fit.measures=TRUE)

#structural regression (observed endogenous variable)
m6c <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
  # regressions
    adjust ~ risk
    read ~ adjust + risk
'
fit6c <- sem(m6c, data=dat)
summary(fit6c, standardized=TRUE, fit.measures=TRUE)
#inspect(fit6c,"partable")

#model6c (manual specification)
m6cc <- '
  # measurement model
    adjust =~ motiv + harm + stabi
    risk =~ verbal + ses + ppsych
  #single indicator factor
    readf =~ 1*read
  #residuel variance to zero
    read ~~ 0*read
  # regressions
    adjust ~ risk
    readf ~ adjust + risk
'
fit6cc <- sem(m6cc, data=dat, optim.method=list("BFGS"))
summary(fit6cc)
#inspect(fit6cc,"partable")
