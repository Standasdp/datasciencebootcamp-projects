#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/ML/ML-Lives2")
#getwd()
# caret = Classification And Regression Tree
install.packages("caret")
install.packages(c("rpart", "ranger", "glmnet", "mlbench"))
install.packages(c("randomForest", "class"))
install.packages("MLmetrics")
install.packages("forcats")
install.packages("rpart.plot")

library(tidyverse)
library(caret)
library(rpart)
library(ranger)
library(glmnet)
library(mlbench)   #ML data set package
library(randomForest)
library(class)
library(MLmetrics)
library(forcats)
library(rpart.plot)

## see available data
data("BostonHousing")

## glimpse data
df <- BostonHousing
glimpse(df)

## KMeans clustering
## clustering => segmentation

subset_df <- df %>%
  select(crim, rm, age, lstat, medv) %>%
  as_tibble()  # default 10 row # Preview top 10

## test different k (k = 2-5) # k is cluster
kmeans(x = subset_df, centers = 2)
result <- kmeans(x = subset_df, centers = 3)


## cluster [1, 2, 3] # Add cluster column in subset_df
subset_df$cluster <- result$cluster

subset_df %>% 
  summarise(avg_medv = mean(medv))

subset_df %>% 
  group_by(cluster) %>%
  summarise(avg_medv = mean(medv))

View(subset_df)


## ----------------------------------------------------------------------------
## KNN Euclidean Distance Manual run
p1 <- c(2,3,5)
p2 <- c(6,8,4)
(d <- sqrt(sum((p1-p2)^2)))

## KNN Auto run
## lm, knn
df <- as_tibble(df)
df


# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

glimpse(train_data)
head(train_data)


# 2. train model
# medv = f(crim, rm, age)
# preProcess = c("center", "scale") : standardization
set.seed(42)
lm_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "lm",
                   preProcess = c("center", "scale"))

lm_model$finalModel


## Choose accuracy optimal
## RMSE optimal
set.seed(42)
knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "RMSE",
                   preProcess = c("center", "scale"))


## R-squared optimal
set.seed(42)
knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   preProcess = c("center", "scale"))



## Tuning
## tune hyperparameters

set.seed(42)

# Resampling
ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

## grid search : tune hyperparameters
k_grid <- data.frame(k = c(3, 5, 7, 9, 11))

knn_model <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneGrid = k_grid,
                   preProcess = c("center", "scale"),
                   trControl = ctrl)

## tuneLength : random search
knn_model1 <- train(medv ~ crim + rm + age,
                   data = train_data,
                   method = "knn",
                   metric = "Rsquared",
                   tuneLength = 5,
                   preProcess = c("center", "scale"),
                   trControl = ctrl)


# 3. score
p <- predict(knn_model, newdata = test_data)


# 4. evaluate
RMSE(p, test_data$medv)



## ------------------------------------------------------------------------------

## classification problem
data("PimaIndiansDiabetes")
glimpse(PimaIndiansDiabetes)

df <- PimaIndiansDiabetes


subset_df <- df %>%
  select(glucose, insulin, age, diabetes)

# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- subset_df[id, ]
test_data <- subset_df[-id, ]



# 2. train model
## medical
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "knn",
                   metric = "Sens", #Recall
                   trControl = ctrl))

# --------------------------------------------------
## medical
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = twoClassSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "knn",
                    metric = "Spec",
                    trControl = ctrl))

# --------------------------------------------------

## Stat
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = prSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "knn",
                    metric = "Recall",
                    trControl = ctrl))

# --------------------------------------------------
## Stat
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = prSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "knn",
                    metric = "AUC",
                    trControl = ctrl))

# --------------------------------------------------
## Stat
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = prSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "knn",
                    metric = "F",
                    trControl = ctrl))


# 3. score
p <- predict(knn_model, newdata = test_data)


# 4. evaluate
table(test_data$diabetes, p, dnn = c("Actual",
                                     "Prediction"))  # manual calculate

## medical
confusionMatrix(p, test_data$diabetes, 
                positive = "pos")

## stat
confusionMatrix(p, test_data$diabetes, 
                positive = "pos", 
                mode = "prec_recall")


## Sensitivity = Recall 32/55
## Specificity = TN/(TN+FP) 76/99



## ---------------------------------------------------------
## relevel diabetes Case ##pos neg
## Predict class Up

## classification problem

data("PimaIndiansDiabetes")
glimpse(PimaIndiansDiabetes)

df <- PimaIndiansDiabetes

## library(forcats) : relevel focus group > in this case focus positive diabetes
df$diabetes <- fct_relevel(df$diabetes, "pos")


subset_df <- df %>%
  select(glucose, insulin, age, diabetes)

# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- subset_df[id, ]
test_data <- subset_df[-id, ]

train_data$diabetes
contrasts(train_data$diabetes)


# 2. train model
## Stat
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE,
                     summaryFunction = prSummary,
                     classProbs = TRUE)

(knn_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "knn",
                    metric = "F",
                    preProcess = c("center", "scale"),
                    trControl = ctrl))



# 3. score
p <- predict(knn_model, newdata = test_data)


# 4. evaluate
table(test_data$diabetes, p, dnn = c("Actual",
                                     "Prediction"))  # manual calculate

confusionMatrix(p, test_data$diabetes, 
                positive = "pos", 
                mode = "prec_recall")



# --------------------------------------------------
# knn accuracy
# for compare models

#set.seed(42)

#ctrl <- trainControl(method = "cv",
#                     number = 5,
#                     verboseIter = TRUE)
#(knn_model <- train(diabetes ~ .,
#                    data = train_data,
#                    method = "knn",
#                    metric = "Accuracy",
#                    trControl = ctrl))


## -----------------------------------------------------------------------------

## Logistic Regression

set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

(logit_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "glm",
                    metric = "Accuracy",
                    trControl = ctrl))

logit_model$finalModel

## test model
p <- predict(logit_model, newdata = test_data)
accuracy <- mean(p == test_data$diabetes)


## ------------------------------------------------------------------------------
## Decision Tree (rpart)

(tree_model <- train(diabetes ~ .,
                    data = train_data,
                    method = "rpart",
                    metric = "Accuracy",
                    trControl = ctrl))

tree_model$finalModel
rpart.plot(tree_model$finalModel)




## -----------------------------------------------------------------------------
## Random Forest
## Model accuracy the highest>= 75%

## mtry = number of feathers used to train model
## bootstrap sampling

data("PimaIndiansDiabetes")
glimpse(PimaIndiansDiabetes)

df <- PimaIndiansDiabetes

## library(forcats) : relevel focus group > in this case focus positive diabetes
df$diabetes <- fct_relevel(df$diabetes, "pos")


# 1. split data
set.seed(42)
n <- nrow(df)
id <- sample(1:n, size = 0.8*n)
train_data <- df[id, ]
test_data <- df[-id, ]

glimpse(train_data)


## bagging metric
mtry_grid <- data.frame(mtry = 2:8)
set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

(rf_model <- train(diabetes ~ .,
                     data = train_data,
                     method = "rf",
                     metric = "Accuracy",
                     tuneGrid = mtry_grid,
                     trControl = ctrl))



## ----------------------------------------------------------------------
## compare models
list_model <- list(knn = knn_model,
                   logistic = logit_model,
                   decisionTree = tree_model,
                   randomForest = rf_model)

result <- resamples(list_model)
summary(result)

modelCor(result)

dotplot(result)


## -------------------------------------------------------------------
## Ridge vs. Lasso Regression
library(glmnet)

# 0 = Ridge, 1 = Lasso

set.seed(42)

ctrl <- trainControl(method = "cv",
                     number = 5,
                     verboseIter = TRUE)

glmnet_grid <- expand.grid(alpha = 0:1,
                          lambda = c(0.1, 0.2, 0.3))

(glmnet_model <- train(diabetes ~ .,
                   data = train_data,
                   method = "glmnet",
                   metric = "Accuracy",
                   tuneGrid = glmnet_grid,
                   trControl = ctrl))
























