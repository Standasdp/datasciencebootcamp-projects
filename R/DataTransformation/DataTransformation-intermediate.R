#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/Data-Transformation")
#getwd()

## install packages
##  tidyverse include : dplyr tidyr ggplot2
install.packages("tidyverse")
library(tidyselect)


## data.frame vs. tibble
df_tibble <- tibble(id = 1:3, name = c("toy", "jisoo", "lisa"))      # show data type & show 10 row first & beautiful more dataframe
df_tibble
class(df_tibble)
df <- data.frame(id = 1:3, name = c("toy", "jisoo", "lisa"))
df
class(df)


## convert dataframe to tibble
mtcars
class(mtcars)
mtcars_tibble <- tibble(mtcars)
mtcars_tibble
class(mtcars_tibble)




## --------------------------------------------------------------------------------
## Sample Data
## sample_n

sample_n(mtcars, size = 5)       #result will random and change every time run

# set.seed() = set result always the same
set.seed(25)
sample_n(mtcars, size = 5)

# random 50% of mtcars data 
sample_frac(mtcars, size = 0.5, replace = T)     #replace = T : result can repeat





## --------------------------------------------------------------------------------
## Slice
mtcars %>%
  slice(1:5)

mtcars %>%
  slice(6:10)

mtcars %>%
  slice(c(1,3,5))

mtcars %>%
  slice(sample(nrow(mtcars), 10))











