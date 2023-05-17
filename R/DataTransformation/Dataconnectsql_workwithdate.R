#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/Data-Viz/Data-viz-Lives")
#getwd()


## Package
install.packages(c("tidyverse", "RSQLite", "RPostgreSQL", "lubridate"))
install.packages("janitor")   ## clean data Package

## library tidyverse
library(tidyverse)
library(lubridate)
library(RSQLite)  #DBI
library(RPostgreSQL)
library(janitor)

list.files()

## connect database
con <- dbConnect(SQLite(), "chinook.db")
con

## list table names
dbListTables(con)

## list fields in a table
dbListFields(con, "customers")

## write SQL queries
df <- dbGetQuery(con, "select * from customers limit 10")
View(df)

df %>%
  select(FirstName, LastName)

## clean_df function in janitor Package
clean_df <- clean_names(df)    #clean data change Upper character to Lower character
View(clean_df)


## write JOIN syntax
df2 <- dbGetQuery(con, "select * from albums, artists
                 where albums.artistid = artists.artistid") %>%
  clean_names()
View(df2)


## write a table
dbWriteTable(con, "cars", mtcars)
dbListTables(con)

dbGetQuery(con, "select * from cars limit 5;")

dbRemoveTable(con, "cars")    #Drop cars
dbListTables(con)


## close connection database
dbDisconnect(con)



## --------------------------------------------------------------------
## connect database PostgreSQL
con1 <- dbConnect(PostgreSQL(),
          host = "arjuna.db.elephantsql.com", #server
          port = 5432,  #default
          user = "bjlnjjwx",
          pass = "jdQ1-fKz6BVal_iW6O_of1MavZIBBxGU",
          dbname = "bjlnjjwx")
con1
dbListTables(con1)


## write table
dbWriteTable(con1, "cars", mtcars %>% slice(1:5))
dbListTables(con1)

## get query
dbGetQuery(con1, "select count(*) from cars")
dbGetQuery(con1, "select * from cars")

## disconnect
dbDisconnect(con1)


## --------------------------------------------------------------------
## Working with date
library(lubridate)

## YYYY-MM-DD
date_df <- data.frame(
  x = c(
    "2023-02-25",
    "2023-02-26",
    "2023-02-27",
    "2023-02-28",
    "2023-03-01"))
class(date_df$x)


date_df %>%
  mutate(date_x = ymd(x),
         year = year(date_x),
         month = month(date_x),
         day = day(date_x),
         wday = wday(date_x))

date_df %>%
  mutate(date_x = ymd(x),
         year = year(date_x),
         month = month(date_x, label = T),
         day = day(date_x),
         wday = wday(date_x, label = T))

date_df %>%
  mutate(date_x = ymd(x),
         year = year(date_x),
         month = month(date_x, label = T, abbr = F),
         day = day(date_x),
         wday = wday(date_x, label = T, abbr = F))

date_df %>%
  mutate(date_x = ymd(x),
         year = year(date_x),
         month = month(date_x, label = T, abbr = F),
         day = day(date_x),
         wday = wday(date_x, label = T, abbr = F),
         week = week(date_x))



## Excel default USA date
date_df <- data.frame(
  x = c(
    "02/25/2023",
    "02/26/2023",
    "02/27/2023",
    "02/28/2023",
    "03/01/2023"))   ## MM/DD/YYYY mdy

date_df %>%
  mutate(date_x = mdy(x),
         year = year(date_x),
         month = month(date_x, label = T, abbr = F),
         day = day(date_x),
         wday = wday(date_x, label = T, abbr = F),
         week = week(date_x))



## Excel default USA date
date_df <- data.frame(
  x = c(
    "Feb 2023 - 25",
    "Feb 2023 - 26",
    "Feb 2023 - 27",
    "Mar 2023 - 9",
    "April 2023 - 1")) 

date_df %>%
  mutate(date_x = myd(x),
         year = year(date_x),
         month = month(date_x, label = T, abbr = F),
         day = day(date_x),
         wday = wday(date_x, label = T, abbr = F),
         week = week(date_x))


## -------------------------------------------
## build function to use
convert_thai_to_eng_date <- function(year) {
  return(year-543)
}
convert_thai_to_eng_date(2566)

##---------------------------------------------
ydm("2023 .. 25 .. FEB")
dmy("25FEB2023")
dmy(c("25FEB2023","26   FEB    2023"))









































