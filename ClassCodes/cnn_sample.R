#' Convolutional NNet: based on chatGPT suggestion
#' # Import the keras library
library(keras)

# Load the MNIST dataset of handwritten digits
## xy <-  dataset_mnist() # fails due to SSL certification
## Download 'https://storage.googleapis.com/tensorflow/tf-keras-datasets/mnist.npz' manually
library(reticulate)
np <- import("numpy")
xy <- np$load(file = file.choose()) # loading 'mnist.npz' loaclly
# elements in mnist.npz
xy$files

x_train <- xy$f[["x_train"]]
y_train <- xy$f[["y_train"]]
x_test <- xy$f[["x_test"]]
y_test <- xy$f[["y_test"]]

# Reshape the input data to have a channel dimension
x_train <- array_reshape(x_train, c(nrow(x_train), 28, 28, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), 28, 28, 1))

# Normalize the input data to be between 0 and 1
x_train <- x_train / 255
x_test <- x_test / 255

# Convert the labels to one-hot encoding
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Build a sequential model with 3 convolutional layers and 2 fully-connected layers
model <- keras_model_sequential() %>%
  # The first convolutional layer with 32 filters of size 3x3 and ReLU activation
  layer_conv_2d(filters = 32, kernel_size = c(3, 3), activation = "relu", input_shape = c(28, 28, 1)) %>%
  # The second convolutional layer with 64 filters of size 3x3 and ReLU activation
  layer_conv_2d(filters = 64, kernel_size = c(3, 3), activation = "relu") %>%
  # The third convolutional layer with 128 filters of size 3x3 and ReLU activation
  layer_conv_2d(filters = 128, kernel_size = c(3, 3), activation = "relu") %>%
  # A max pooling layer with pool size of 2x2
  layer_max_pooling_2d(pool_size = c(2, 2)) %>%
  # A dropout layer with probability of 0.25 to reduce overfitting
  layer_dropout(rate = 0.25) %>%
  # A flatten layer to convert the 4D output to 2D
  layer_flatten() %>%
  # A fully-connected layer with 256 neurons and ReLU activation
  layer_dense(units = 256, activation = "relu") %>%
  # A dropout layer with probability of 0.5 to reduce overfitting
  layer_dropout(rate = 0.5) %>%
  # A fully-connected layer with 10 neurons and softmax activation for output probabilities
  layer_dense(units = 10, activation = "softmax")

# Compile the model with categorical crossentropy loss and adam optimizer
model %>% compile(
  loss = "categorical_crossentropy",
  optimizer = "adam",
  metrics = c("accuracy")
)

# Train the model for 10 epochs with batch size of 128
model %>% fit(
  x = x_train,
  y = y_train,
  epochs = 10,
  batch_size = 128,
  validation_split = .2
)

# Evaluate the model on the test set
model %>% evaluate(x_test, y_test)

# Predictions
def.par <- par(no.readonly = TRUE)
par(mar=c(0,0,0,0), mfrow=c(3,3))
k <- 1:9
prds <- model$predict(x_test[k,,,,drop=FALSE])
prds <- apply(prds, 1, which.max)-1
for(i in k){
  image(x_test[i,,,], asp=1, axes=FALSE)
  title(main = prds[k[i]], line = -1)
}
