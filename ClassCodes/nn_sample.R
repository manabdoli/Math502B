# Sample NeuralNet Code
# Load the neuralnet library
library (neuralnet)

# Load the iris dataset
data (iris)

# Normalize the data using min-max scaling
normalize <- function (x) {
  return ( (x - min (x)) / (max (x) - min (x)))
}

iris_norm <- as.data.frame (lapply (iris[,1:4], normalize))
iris_norm$Species <- iris$Species

# Split the data into training and testing sets
set.seed (123)
ind <- sample (2, nrow (iris), replace = TRUE, prob = c (0.7, 0.3))
train <- iris_norm [ind == 1,]
test <- iris_norm [ind == 2,]

# Train a neural network with 4 inputs, 5 hidden neurons and 3 outputs
nn <- neuralnet (Species ~ Sepal.Length + Sepal.Width + Petal.Length + Petal.Width,
                 data = train, hidden = 5, err.fct = "ce", linear.output = FALSE)

# Plot the neural network
plot (nn)

# Predict the species for the test set
pred <- compute (nn, test [,1:4])

# Convert the predictions to factor levels
pred_factor <- factor (max.col (pred$net.result), labels = levels (test$Species))

# Calculate the accuracy of the predictions
accuracy <- sum (pred_factor == test$Species) / nrow (test)
print (paste ("Accuracy:", accuracy))
