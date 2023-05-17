#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/Data-Transformation")
#getwd()

## install new package in RStudio
install.packages("dplyr")

## load library
library(dplyr)


## Read CSV file
imdb <- read.csv("imdb.csv", stringsAsFactors = F)
View(imdb)


## review data structure
glimpse(imdb)

## print head and tial of data
head(imdb, 10)
tail(imdb, 10)


## --------------------------------------------------------------------------
## select column
names(imdb)
select(imdb, MOVIE_NAME, RATING)
select(imdb, 1, 5)

select(imdb, movie_name = MOVIE_NAME, released_year = YEAR)  ### Same


## pipe operator
head(imdb)
imdb %>% head()
imdb %>% 
  select(movie_name = MOVIE_NAME, released_year = YEAR) %>%
  head(10)                                                   ### Same





## --------------------------------------------------------------------------
## filter data part I
filter(imdb, SCORE >= 9.0)

imdb %>% filter(SCORE >= 9.0)  #above same below

### change column name to Lower-case
names(imdb) <- tolower(names(imdb))
head(imdb)

### filter example
imdb %>%
  select(movie_name, year, score) %>%
  filter(score >= 9)

imdb %>%
  select(movie_name, year, score) %>%
  filter(score >= 9 & year > 2000)                 # filter multi condition and

imdb %>%
  select(movie_name, length, score) %>%
  filter(score == 8.8)

imdb %>%
  select(movie_name, length, score) %>%
  filter(score == 8.8 | score == 8.3)              # filter multi condition or

imdb %>%
  select(movie_name, length, score) %>%
  filter(score == 8.8 | score == 8.3 | score == 9)     ## below same above

imdb %>%
  select(movie_name, length, score) %>%
  filter(score %in% c(8.3, 8.8, 9.0))                  ## above same below  #%in% = in operater 



## --------------------------------------------------------------------------
## filter data part II
## filter string columns
imdb %>%
  select(movie_name, genre, rating) %>%
  filter(rating == "R")                   #filter R rating movie

imdb %>%
  select(movie_name, genre, rating) %>%
  filter(rating == "PG-13")               #filter PG-13 rating movie

imdb %>%
  select(movie_name, genre, rating) %>%
  filter(genre == "Drama")               #filter specifically Drama genre movie


imdb$genre
### grepl() is case sensitive
grepl("Drama", imdb$genre)    #use to search pattern : return T if match and F if not match

imdb %>%
  select(movie_name, genre, rating) %>%
  filter(grepl("Drama", imdb$genre))               #filter all Drama genre movie


imdb %>%
  select(movie_name) %>%
  filter(grepl("The", imdb$movie_name))           #filter "The" in movie name


imdb %>%
  select(movie_name) %>%
  filter(grepl("King", imdb$movie_name))           #filter "King" in movie name




## --------------------------------------------------------------------------
## Create New Column
imdb %>%
  select(movie_name, score) %>%
  mutate(score_group = if_else(score >= 9, "High Rating", "Low Rating")) %>%
  head(10)


imdb %>%
  select(movie_name, score, length) %>%
  mutate(score_group = if_else(score >= 9, "High Rating", "Low Rating"),
         length_group = if_else(length >= 120, "Long Film", "Short Film")) %>%
  head(10)


imdb %>%
  select(movie_name, score) %>%
  mutate(score_update = score + 0.1) %>%
  head(10)


imdb %>%
  select(movie_name, score) %>%
  mutate(score = score + 0.1) %>%
  head(10)                           #can overwrite original column




## --------------------------------------------------------------------------
## Arrange Data
head(imdb)

# ASC
imdb %>%
  arrange(length) %>%
  head(10)

# DESC
imdb %>%
  arrange(desc(length)) %>%
  head(10)

# sort multi column
imdb %>%
  arrange(rating, desc(length)) %>%
  head(20)                             # rating sort ASC , length sort DESC





## --------------------------------------------------------------------------
## Summary Statistics and Group by

imdb %>%
  summarise(mean_length = mean(length),
            sum_length = sum(length),
            sd_length = sd(length),
            min_length = min(length),
            max_length = max(length),
            n = n())


imdb %>%
  group_by(rating) %>%
  summarise(mean_length = mean(length),
            sum_length = sum(length),
            sd_length = sd(length),
            min_length = min(length),
            max_length = max(length),
            n = n())


imdb %>%
  filter(rating != "") %>%
  group_by(rating) %>%
  summarise(mean_length = mean(length),
            sum_length = sum(length),
            sd_length = sd(length),
            min_length = min(length),
            max_length = max(length),
            n = n())                           #filter blank out




## --------------------------------------------------------------------------
## Join Tables
head(imdb)

favorite_films <- data.frame(id = c(5, 10, 25, 30, 98))
favorite_films

# inner_join(table_name, by("PK1" = "PK2"))
favorite_films %>%
  inner_join(imdb, by = c("id" = "no"))   #join imdb column no = favorite_film column id




## --------------------------------------------------------------------------
## Export CSV file
imdb_prep <- imdb %>%
  select(movie_name, released_year = year, rating, length, score) %>%
  filter(rating == "R" & released_year > 2000)

imdb_prep

## export file
write.csv(imdb_prep, "imdb_prep.csv", row.names = FALSE)

























