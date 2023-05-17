## Homework House Price India
#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/ML/ML-HW")
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


## Prep Data
df <- read.csv("House-Price-India.csv")
View(df)
sum(is.na(df)*1)
glimpse(df)

names(df)
select_columns <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 19, 20, 21, 22 )
new_names <- c("num_bed", "num_bath", "living_area", "lot_area", 
               "num_floor", "waterfront", "num_view", "cond", "grade", "area_ex_base", 
               "area_base", "b_year", "re_year", "liv_area_re", "lot_area_re",
               "school_near", "dist_airport")
names(df)[select_columns] <- new_names


## Correlation
cordat <- df %>%
  select(Price, num_bed, num_bath, living_area,
         lot_area, num_floor, waterfront, num_view,
         cond, grade, area_ex_base, area_base,
         b_year, liv_area_re, lot_area_re, school_near,
         dist_airport) %>%
  cor()
View(cordat)
hist(df$Price)

## Data Transformation # normalize
log_Price <- log(df$Price + 1)
hist(log_Price)

## 1. split data 80% : 20%
## define function
train_test_split <- function(data, trainRatio=0.7) {
  set.seed(42)
  (n <- nrow(data))
  (id <- sample(1:n, size = trainRatio*n))
  
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return( list(train = train_data, test= test_data) )
}

splitData <- train_test_split(df, 0.8)
train_data <- splitData$train
test_data <- splitData$test


## 1.1 Data Transformation # normalize
train_data$Price_norm <- log(train_data$Price + 1)
test_data$Price_norm <- log(test_data$Price + 1)


# 2. train Model

set.seed(42)

ctrl <- trainControl(
  method = "cv", # k-fold golden standard
  number = 5, # k = 5
  verboseIter = TRUE
)

lmmodel <- train(Price_norm ~ num_bath + living_area + num_view + 
                 grade + area_ex_base + area_base + liv_area_re, 
                 data = train_data, 
                 method = "lm", 
                 trControl = ctrl)

rfmodel <- train(Price_norm ~ num_bath + living_area + num_view + 
                   grade + area_ex_base + area_base + liv_area_re, 
                 data = train_data, 
                 method = "rf", 
                 trControl = ctrl)

knnmodel <- train(Price_norm ~ num_bath + living_area + num_view + 
                   grade + area_ex_base + area_base + liv_area_re, 
                 data = train_data, 
                 method = "knn", 
                 trControl = ctrl)


# 3. score Model
lmpred <- predict(lmmodel, newdata = test_data)
rfpred <- predict(rfmodel, newdata = test_data)
knnpred <- predict(knnmodel, newdata = test_data)





# 4. evaluate Model
# MAE
mae_metric <- function(actual, prediction) {
  # mean absolute error
  abs_error <- abs(actual - prediction)
  mean(abs_error)
}


# MSE
mse_metric <- function(actual, prediction) {
  # mean squared error
  sq_error <- (actual - prediction)**2
  mean(sq_error)
}


# RMSE 
rmse_metric <- function(actual, prediction) {
  # Root mean squared error
  sq_error <- (actual - prediction)**2
  sqrt(mean(sq_error))  ##back to normal unit
}


rmse_metric(test_data$Price_norm, lmpred)
rmse_metric(test_data$Price_norm, rfpred)
rmse_metric(test_data$Price_norm, knnpred)

