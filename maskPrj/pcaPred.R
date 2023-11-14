# PCA ####
library(Math502B)


# Data ####
if(!grepl(pattern = 'maskPrj$', getwd())) setwd('maskPrj')
if(!exists('mask2m')) load('mask2m.rda')
thinBy <- 10
ijs <- thinIJs(mask2m, thinBy)
mask2m <- mask2m[ijs$Is, ijs$Js, ]
nRow <- dim(mask2m)[1]
nCol <- dim(mask2m)[2]
dim(mask2m) <- c(prod(dim(mask2m)[1:2]), dim(mask2m)[3])

write.csv(mask2m, file = 'MaskThinedBy10_31_31.csv')
pcaMsk <- prcomp(mask2m)

pca_spree(pcaMsk, xlim=c(1, 10))

summary(pcaMsk)$importance[,1:10]

## 4 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:4], centers = 4)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

## 5 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:4], centers = 5)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

## 3 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:4], centers = 3)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

## 6 Clusters
kmCluster <- kmeans(pcaMsk$x[,1:4], centers = 6)
ij <- k2ij(nRow, 1:dim(mask2m)[1])
plot(ij[,1], ij[,2], col=kmCluster$cluster)

# Prediction:
# scale(mask2m, center = pcaMsk$center, scale = pcaMsk$scale)%*%pcaMsk$rotation[, 1:4]-
#  predict(pcaMsk, newdata = mask2m)[,1:4]

# Using PCA in training nnet ####
# Load the keras library
library(keras)

if(!exists('maskData')) load('MaskData.rda')

# Split the data into training and testing sets
set.seed(123)
ind <- sample(2, nrow(maskData$x), replace = TRUE, prob = c(0.7, 0.3))
train_pos <- maskData$pos[maskData$y[ind == 1],]
test_pos <- maskData$pos[maskData$y[ind == 2],]

x_train_pca <- prcomp(maskData$x[ind==1,])
pca_spree(x_train_pca, xlim=c(1,10))
summary(x_train_pca)$importance[, 1:10]

x_train <- x_train_pca$x[,1:4]

x_test <- scale(maskData$x[ind==2,], x_train_pca$center, x_train_pca$scale) %*%
                  x_train_pca$rotation[, 1:4]

# train PCA

# Convert the labels to one-hot encoding
y_train_pos1 <- to_categorical(train_pos[,1]-1)
y_train_pos2 <- to_categorical(train_pos[,2]-1)
y_test_pos1 <- to_categorical(test_pos[,1]-1)
y_test_pos2 <- to_categorical(test_pos[,2]-1)

# Build a sequential model with 4 inputs, 16 hidden neurons and 3 outputs
model <- keras_model_sequential() %>%
  layer_dense(units = 6, activation = "relu", input_shape = c(4)) %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(6)) %>%
  layer_dense(units = 3, activation = "softmax")

# Compile the model with categorical crossentropy loss and adam optimizer
model %>% compile(
  loss = "categorical_crossentropy", # -1/N Sum_i{1 tp N}(Sum_j{1:C}(y_ij * log(p_ij))):
  # y_ij is one-hot coding of the class lable; p_ij is the association to class j
  optimizer = "adam",
  metrics = c("accuracy")
)

# Train the model for 100 epochs with batch size of 32
history <- model %>% fit(
  x = as.matrix(train[,1:4]),
  y = y_train,
  epochs = 100,
  batch_size = 32,
  validation_split = 0.2
)

# Evaluate the model on the test set
model %>% evaluate(as.matrix(test[,1:4]), y_test)
