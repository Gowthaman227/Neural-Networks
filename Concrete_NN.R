concrete <- read.csv(file.choose())
View(concrete)
# Normalization function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

# Apply normalization to entire data frame
concrete_norm <- as.data.frame(lapply(concrete, normalize))
View(concrete_norm)

# create training and test data
concrete_train <- concrete_norm[1:773, ]
View(concrete_train)
concrete_test <- concrete_norm[774:1030, ]
View(concrete_test)

## Buliding a model on the training data
library(neuralnet)
# simple ANN with only a single hidden neuron
concrete_model <- neuralnet(formula = strength ~ cement + slag +
                              ash + water + superplastic + 
                              coarseagg + fineagg + age,
                            data = concrete_train)


# visualize the network topology
plot(concrete_model,lwd=2)

## Evaluating model performance 
## Obtain model results

concrete_test[1:8]
results_model <- compute(concrete_model, concrete_test[1:8])
results_model
# Obtain predicted strength values
str(results_model)
predicted_strength <- results_model$net.result
predicted_strength
# Examine the correlation between predicted and actual values
cor(predicted_strength, concrete_test$strength)
## Improving model performance ----
# a more complex neural network topology with 10 hidden neurons
concrete_model2 <- neuralnet(strength ~ cement + slag +
                               ash + water + superplastic + 
                               coarseagg + fineagg + age,
                             data = concrete_train, hidden = 10)


# plot the neural network
plot(concrete_model2,lwd=2)

# evaluate the results again
model_results2 <- compute(concrete_model2, concrete_test[1:8])
predicted_strength2 <- model_results2$net.result
cor(predicted_strength2, concrete_test$strength)
