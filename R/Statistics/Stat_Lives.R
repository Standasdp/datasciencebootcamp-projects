#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/Stat")
#getwd()
## 04-03-23
library(tidyverse)

## Central Limit Theorem
true_mean <- mean(diamonds$price)
true_mean   #3932.8

results <- replicate(5000, {
  diamonds %>%
    sample_n(2000) %>%
    summarise(
      mean_price = mean(price)) %>%
    pull(mean_price)
    
})

hist(results)
mean(results)


## ----------------------------------------------------------------------------
## 05-03-23
## standardization (Z-score)
## [-3,3]
z_calculate <- function(x) {
  (x- mean(x)) / sd(x)
}

z_calculate(c(1,2,3,4,5))
z_calculate(mtcars$hp)


## normalization (min-max normalization)
## [0-1] feature scaling

norm_cal <- function(x) {
  (x- min(x)) / (max(x) - min(x))
}

norm_cal(1:5)
norm_cal(mtcars$hp)


## -----------------------------------------------

## Confidence interval for hp
mean(mtcars$hp); sd(mtcars$hp)  ## SD Sample

sd_pop <- function(x) {
  sqrt(sum((x - mean(x))**2)/ (length(x)))
}

sd_pop(mtcars$hp)    ## SD Population
sd(mtcars$hp)        ## SD Sample

## t.test
t.test(mtcars$hp)



## -----------------------------------------------
## create promo A vs. promo B
promo_a <- rnorm(100, mean=550, sd=10)
promo_b <- rnorm(100, mean=400, sd=8)


## H0 : promo a - b = 0
## H1 :  promo a - b != 0

result <- t.test(promo_a, promo_b, alternative = "two.sided")

ifelse(result$p.value <= 0.05, "sig", "not sig")


























