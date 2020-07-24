library(plyr)
startups <- read.csv(file.choose())
View(startups)
str(startups)
startups$State <- as.numeric(revalue(startups$State,
                                     c("New York"="0", "California"="1",
                                       "Florida"="2")))
str(startups)
## Normalization Function
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
## Appling normalization function on dataset removing State column 
startups_norm <- as.data.frame(lapply(startups,normalize))
View(startups_norm)

## Creating training and testing dataset
startups_train <- startups_norm[1:38,]
View(startups_train)
startups_test <- startups_norm[39:50,]
View(startups_test)

## Buliding a Neuralnet model on training dataset
library(neuralnet)
startup_Model1 <- neuralnet(Profit~.,data=startups_train)
summary(startup_Model1)
## Visualization of Neural network
plot(startup_Model1)
## Evaluating model Performance
Model1_results <- compute(startup_Model1,startups_test)
Model1_results
## Obtaining predicted profit values
Profit_pred <- Model1_results$net.result
Profit_pred
## Examine Correlation b/w predicted and actual values
cor(Profit_pred,startups_test$Profit)

## Improving Model Performance by increasing hidden layers
startup_Model2 <- neuralnet(Profit~.,data=startups_train,hidden=10) 
summary(startup_Model2)
## Visualization of Neural network
plot(startup_Model2)
## Evaluating model Performance
Model2_results <- compute(startup_Model2,startups_test)
Model2_results
## Obtaining predicted profit values
Profit_pred1 <- Model2_results$net.result
Profit_pred1
## Examine Correlation b/w predicted and actual values
cor(Profit_pred1,startups_test$Profit)
