#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/ML")
#getwd()
# caret = Classification And Regression Tree
install.packages("caret")
library(tidyverse)
library(caret)

## train test split
## 1. split data
## 2. train
## 3. score
## 4. evaluate

glimpse(mtcars)

# split data 80% : 20%
train_test_split <- function(data, trainRatio=0.7) {
  set.seed(42)
  (n <- nrow(data))
  (id <- sample(1:n, size = trainRatio*n))
  
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return( list(train = train_data, test= test_data) )
}

set.seed(42)
splitData <- train_test_split(mtcars, 0.8)
train_data <- splitData$train
test_data <-splitData$test


# train model
model <- lm(mpg ~ hp + wt + am, data = train_data)


# score model
mpg_pred  <- predict(model, newdata = test_data)


# evaluate model
# MAE, MSE, RMSE

# MAE treat every data point the same
mae_metric <- function(actual, prediction) {
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}
mae_metric(test_data$mpg, mpg_pred)


# MSE treats ตัวที่ทายผิดเยอะๆมากกว่า #unit^2
mse_metric <- function(actual, prediction) {
  # mean squared error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}
mse_metric(test_data$mpg, mpg_pred)


# RMSE 
rmse_metric <- function(actual, prediction) {
  # Root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error))  ##back to normal unit
}
rmse_metric(test_data$mpg, mpg_pred)


## MAE MSE RMSE
actual <- test_data$mpg
prediction <- mpg_pred
mae_metric(actual, prediction)
mse_metric(actual, prediction)
rmse_metric(actual, prediction)



### ------------------------------------------------------------------------
### CARET = Classification And Regression Tree
### Supervised Learning = Prediction
library(caret)

# 1. split Data
splitData <- train_test_split(mtcars, 0.7)
train_data <- splitData[[1]]
test_data <- splitData[[2]]



# 2. train Model
# lm better rf sometimes
# model 230 model
# mpg = f(hp, wt, am)
set.seed(42)

ctrl <- trainControl(
  method = "cv", # k-fold golden standard
  number = 5, # k = 5
  verboseIter = TRUE
)

lmmodel <- train(mpg ~ hp + wt + am, 
               data = train_data, 
               method = "lm", 
               trControl = ctrl)

rfmodel <- train(mpg ~ hp + wt + am, 
                 data = train_data, 
                 method = "rf", # random forest
                 trControl = ctrl)

knnmodel <- train(mpg ~ hp + wt + am, 
                 data = train_data, 
                 method = "knn", 
                 trControl = ctrl)

# rf > lm > knn
# Experimentation

# 3. score Model
p <- predict(model, newdata = test_data)



# 4. evaluate Model
rmse_metric(test_data$mpg, p)



# 5. save model
saveRDS(model, "linear_regression_v1.RDS")



## ----------------------------------------------------------------------------
## Simulate a friend's computer that doesn't have a model but has new data
## and try to test model.
new_cars <- data.frame(
  hp = c(150, 200, 250),
  wt = c(1.25, 2.2, 2.5),
  am = c(0, 1, 1)
)

model <- readRDS("linear_regression_v1.RDS")

new_cars$mpg_pred <- predict(model, newdata = new_cars)
View(new_cars)

## ----------------------------------------------------------------------------
