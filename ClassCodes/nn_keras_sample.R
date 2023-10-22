# Sample NN using keras library (requires installing python)

# Load the keras library
library(keras)

# Load the iris dataset
data(iris)

# Split the data into training and testing sets
set.seed(123)
ind <- sample(2, nrow(iris), replace = TRUE, prob = c(0.7, 0.3))
train <- iris[ind == 1,]
test <- iris[ind == 2,]

# Convert the labels to one-hot encoding
y_train <- to_categorical(as.integer(train$Species)-1)
y_test <- to_categorical(as.integer(test$Species)-1)

# Build a sequential model with 4 inputs, 16 hidden neurons and 3 outputs
model <- keras_model_sequential() %>%
  layer_dense(units = 16, activation = "relu", input_shape = c(4)) %>%
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
