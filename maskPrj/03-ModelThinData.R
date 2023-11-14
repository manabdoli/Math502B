#' Predicting Clusters by NNET
#'
#'  1. create/load random masks
#'  2. split into train and test
#'  3. train
#'  4. test
#'

# 1. Load Noisy Data
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('maskData')) load('NoisyData100thin.rda')
str(maskData)

# 2. Split into train and test
set.seed(123)
trIdx <- sample.int(dim(maskData$x)[1], .80*dim(maskData$x)[1])

# 3. train
# Load the keras library
library(keras)
y_train <- to_categorical(maskData$y[trIdx])
y_test <- to_categorical(maskData$y[-trIdx])
