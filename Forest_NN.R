library(plyr)
forest_fire <- read.csv(file.choose())
View(forest_fire)
str(forest_fire)
## Creating dummy variable for Month and Day 
forest_fire$month <- as.numeric(revalue(forest_fire$month,c("jan"="1","feb"="2",
                     "mar"="3","apr"="4","may"="5","jun"="6","jul"="7","aug"="8",
                     "sep"="9","oct"="10","nov"="11","dec"="12")))
forest_fire$day <- as.numeric(revalue(forest_fire$day,c("mon"="1","tue"="2",
                   "wed"="3","thu"="4","fri"="5","sat"="6","sun"="7")))
str(forest_fire)
## Normalization Function
normalize <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}
## Apply normalization function on dataset 
forest_fire_norm <- as.data.frame(lapply(forest_fire[-31],normalize))
View(forest_fire_norm)
forest_fire_norm <- data.frame(forest_fire[31],forest_fire_norm)
## Create Traning and testing dataset
Forest_train <- forest_fire_norm[1:388,]
View(Forest_train)
Forest_test <- forest_fire_norm[389:517,]
View(Forest_test)
## Build a Neural Network Model
library(neuralnet)
Forest_NN1 <- neuralnet(size_category~.,data=Forest_train)
summary(Forest_NN1)
## Visualization of Neural Network
plot(Forest_NN1,lwd=2)
# Evaluating the model performance
result_NN1 <- compute(Forest_NN1,Forest_test)
result_NN1
## Obtain predicted values
NN1_pred <- result_NN1$net.result
NN1_pred

## Improving Model performance by increasing hidden layers
Forest_NN2 <- neuralnet(size_category~.,data=Forest_train,hidden=5)
summary(Forest_NN2)
## Visualization of Neural Network
plot(Forest_NN2,lwd=2)
# Evaluating the model performance
result_NN2 <- compute(Forest_NN1,Forest_test)
result_NN2
## Obtain predicted values
NN2_pred <- result_NN2$net.result
NN2_pred
