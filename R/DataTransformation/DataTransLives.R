#Start Learning with Bootcamp7 in Data Transformation
#rm(list=ls())
#print("Hello wolrd ")
#print('hello')

## SetWorkDirectory
#setwd("C:/Users/User/Desktop/RBootcamp/Data-Transformation")
#getwd()

## install new package in RStudio
install.packages("dplyr")
install.packages("tidyverse")
install.packages(c("sqldf", "glue"))
install.packages("readr")

## load library
library(dplyr)
library(tidyverse)
library(glue)
library(sqldf)
library(readr)


## explore dataframe
head(mtcars, 5)
tail(mtcars, 5)
glimpse(mtcars)


# sql
# packages "sqldf"
# run sql query with R dataframe
sqldf("select * from mtcars where mpg > 30")
df <- sqldf("select mpg, wt, hp from mtcars where wt < 2")
df
sqldf("select am, avg(mpg), sum(mpg) from mtcars group by am")



# --------------------------------------------------------------------------
#  packages "glue"
#  string template

my_name <- "toy"
my_age <- 34

glue("Hello my name is {my_name}, and I'm {my_age} years old.")

paste("Hello my name is", my_name, ", and I'm",my_age, "year old.")  #hard to write


# --------------------------------------------------------------------------
# tidyverse
# dplyr => data transformation
# 1. select
# 2. filter
# 3. mutate
# 4. arrange
# 5. summarise + group_by


# ------------------------------------------
# select columns
select(mtcars, mpg, hp, wt)
select(mtcars, hp, wt, mpg)
select(mtcars, contains("a"))   #have "a" in head columns
select(mtcars, starts_with("a"))   #start with "a" in head columns
select(mtcars, ends_with("a"))    #end with "a" in head columns
select(mtcars, 1:5, am)    #select from position
select(mtcars, 1:3)
select(mtcars, mpg:disp)



# %>% pipe operator
mtcars %>% 
  select(mpg, hp, wt)


#------------------------------------------
# filter
mtcars %>% 
  select(mpg, hp, wt) %>%
  filter(mpg > 30)


# AND
mtcars %>% 
  select(mpg, hp, wt) %>%
  filter(mpg > 30 & hp > 100)

car_mpg30_hp100 <- mtcars %>% 
  select(mpg, hp, wt) %>%
  filter(mpg > 30 & hp > 100) %>%
  rownames()
car_mpg30_hp100

mtcars %>% 
  select(mpg, hp, wt, am) %>%
  filter(mpg > 30 & am == 1)

##################################### LEARN STOP ###############################

#OR
mtcars %>% 
  select(mpg, hp, wt) %>%
  filter(mpg > 30 | hp > 100)

mtcars %>% 
  select(mpg, hp, wt, am) %>%
  filter(mpg > 30 | am == 1)

mtcars %>% 
  select(mpg, hp, wt, am) %>%
  filter(mpg > 30 | am == 1) %>%
  filter(mpg < 20)

mtcars %>% 
  head() %>%
  rownames()

# rename columns
mtcars %>%
  rownames_to_column() %>%
  select(model = rowname, 
         milePerGallon = mpg,
         horsePower = hp,
         weight = wt) %>%
  head()


mtcars <- mtcars %>%
  rownames_to_column()


mtcars <- mtcars %>%
  rename(model = rowname)
mtcars %>% head()


head(mtcars)



# filter model names
mtcars %>%
  select(model, mpg, hp, wt) %>%
  filter(grepl("^M", model))

grepl("^M", mtcars$model)

mtcars %>%
  select(model, mpg, hp, wt) %>%
  filter(grepl("^Me", model))
 

# mutate create new columns
df <- mtcars %>%
  select(model, mpg, hp) %>%
  head() %>%
  mutate(mpg_double = mpg*2,
         mpg_log = log(mpg),
         hp_double = hp*2)

df



# arrange sort data
mtcars %>% 
  select(model, mpg, am) %>%
  arrange(mpg) %>%
  head(10)

mtcars %>% 
  select(model, mpg, am) %>%
  arrange(desc(mpg)) %>%
  head(10)

mtcars %>% 
  select(model, mpg, am) %>%
  arrange(am, desc(mpg))



# mutata create label
# am (0=auto, 1=manual)
mtcars <- mtcars %>%
  mutate(am = ifelse(am == 0, "Auto", "Manual"))

mtcars %>% head(10)


mtcars %>% 
  select(model, mpg, am) %>%
  arrange(am, desc(mpg))


# create dataframe from scratch
df <- data.frame(
  id = 1:5,
  country = c("Thailand", "Korea", "Japan", "USA", "Belgium")
)

df


df %>%
  mutate(region = case_when(
    country %in% c("Thailand", "Korea", "Japan") ~ "Asia",
    country == "USA" ~ "America",
    TRUE ~ "Other Region"
  ))

df2 <- data.frame(id = 6:8,
                  country = c("Germany", "Italy", "Sweden"))
df
df2



# combine 2 dataframe
full_df <- df %>% bind_rows(df2)
full_df


# combine 3 dataframe
df3 <- data.frame(id = 9:10, country = c("Canada", "Malasia"))
df
df2
df3

## 1
df %>%
  bind_rows(df2) %>%     #same below
  bind_rows(df3)


## 2
list_df <- list(df, df2, df3)
list_df
bind_rows(list_df)            #same above
full_df <- bind_rows(list_df) 
full_df



## Case When in R
full_df %>%
  mutate(region = case_when(
    country %in% c("Thailand", "Korea", "Japan", "Malasia") ~ "Asia",
    country %in% c("Canada", "USA") ~ "America",
    TRUE ~ "Europe"
  ))   #same below



# Case When in SQL use sqldf   #same above
sqldf("select *, case 
            when country in ('USA', 'Canada') then 'America'
            when country in ('Thailand', 'Korea', 'Japan', 'Malasia') then
'Asia'
            else 'Europe'
            end as region
            from full_df
      ")





# ---------------------------------------------------------------------------
# summarise + group_by

mtcars %>%
  summarise(avg_mpg= mean(mpg),
            sum_mpg = sum(mpg),
            min_mpg = min(mpg),
            max_mpg = max(mpg),
            n = n())


mtcars %>%
  group_by(am) %>%
  summarise(avg_mpg= mean(mpg),
            sum_mpg = sum(mpg),
            min_mpg = min(mpg),
            max_mpg = max(mpg),
            n = n())


mtcars %>% select(vs)

## group by more than 1 column
result <- mtcars %>%
  mutate(vs = if_else(vs==0 , "v-shaped", "straight")) %>%
  group_by(am, vs) %>%
  summarise(avg_mpg= mean(mpg),
            sum_mpg = sum(mpg),
            min_mpg = min(mpg),
            max_mpg = max(mpg),
            n = n())
result
View(result)

write_csv(result, "result.csv")
df <- read_csv("result.csv")



# ----------------------------------------------------------------------------
# missing values
# NA (not available)

v1 <-  c(5, 10, 15, NA, 25)
is.na(v1)

data("mtcars")
mtcars[5, 1] <- NA
mtcars

mtcars %>%
  filter(is.na(mpg))    #filter NA in mpg

mtcars %>%
  select(mpg, hp, wt) %>%
  filter(is.na(mpg))    #filter NA

mtcars %>%
  select(mpg, hp, wt) %>%
  filter(!is.na(mpg))   #filter complete case


# replace NA use by mean
mtcars %>%
  summarise(avg_mpg = mean(mpg))     #find mean = NA because have NA, So del NA before

mtcars %>%
  summarise(avg_mpg = mean(mpg, na.rm = TRUE))    #find mean(mpg) but before find must remove na out by use (na.rm = T)

mtcars %>%
  filter(!is.na(mpg)) %>%
  summarise(avg_mpg = mean(mpg))    # same above : filter na out before find mean(mpg)


## mean imputation #replace NA use by mean
mean_mpg <- mtcars %>%
  summarise(mean(mpg, na.rm=T)) %>%
  pull()
mean_mpg         # set mean_mpg values

mtcars %>%
  select(mpg) %>%
  mutate(mpg2 = replace_na(mpg, mean_mpg))     #Replace mean_mpg values in NA



#----------------------------------------------------------------------------
# loop over dataframe  # in Python use for loop but #in R use apply 

data("mtcars")    #original dataframe

mtcars

# 1 = row, 2 = column
apply(mtcars, 2, mean)      #make sence   # apply mean all column in mtcars
apply(mtcars, 2, sum)       #make sence   # apply sum all column in mtcars
apply(mtcars, 1, mean)      #not make sence  # apply mean all row in mtcars
apply(mtcars, 1, sum)       #not make sence  # apply sum all row in mtcars





#----------------------------------------------------------------------------
# join dataframe
# standard joins in SQL (inner, left, right, full)

band_members             #data in R
band_instruments         #data in R


left_join(band_members,band_instruments)   #same below
left_join(band_members,band_instruments ,by="name")   #same above

band_members %>%
  left_join(band_instruments ,by="name")

band_members %>%
  inner_join(band_instruments ,by="name")

band_members %>%
  right_join(band_instruments ,by="name")

band_members %>%
  full_join(band_instruments ,by="name")



band_members
band_instruments

# rename column
band_members %>%
  rename(memberName = name) -> band_members2
band_members2

band_members2 %>%
  left_join(band_instruments, by = c("memberName" = "name"))





#----------------------------------------------------------------------------
install.packages("nycflights13")
library(nycflights13)        #data flights in Newyork

# Table flights in "nycflights13"
View(flights)
glimpse(flights)

flights %>%
  filter(year == 2013 & month == 9) %>%
  count(carrier)                            #frequency table


flights %>%
  filter(year == 2013 & month == 9) %>%
  count(carrier) %>%
  arrange(n)                                #ASC

flights %>%
  filter(year == 2013 & month == 9) %>%
  count(carrier) %>%
  arrange(-n)                               #DESC

flights %>%
  filter(year == 2013 & month == 9) %>%
  count(carrier) %>%
  arrange(-n) %>%
  head(5)                                   #top 5



# Table airlines in "nycflights13"
glimpse(airlines)

# join
flights %>%
  filter(year == 2013 & month == 9) %>%
  count(carrier) %>%
  arrange(-n) %>%
  head(5) %>%
  left_join(airlines, by="carrier")






#----------------------------------------------------------------------------
# use web scrapping in R
install.packages("rvest")
install.packages("tidyverse")
library(rvest)
library(tidyverse)

# use only static website
url <- "https://www.imdb.com/search/title/?groups=top_100&sort=user_rating%2Cdesc"

movie_name <- url %>%
  read_html() %>%
  html_nodes("h3") %>%
  html_text2()        #remove charactor not use

movie_name



movie_name <- url %>%
  read_html() %>%
  html_elements("h3.lister-item-header") %>%
  html_text2()

movie_name



rating <- url %>%
  read_html() %>%
  html_elements("div.ratings-imdb-rating") %>%
  html_text2() %>%
  as.numeric()

rating


votes <- url %>%
  read_html() %>%
  html_elements("p.sort-num_votes-visible") %>%
  html_text2()

votes

df <- data.frame(movie_name, rating, votes)
View(df)

IMDB_view <- df %>%
  separate(votes, sep=" \\| ", into=c("votes", "gross", "tops")) %>%
  View()
IMDB_view


# Homework IMDB > movie name & rating : ^ above
# Homework Static website : below
## 1

datarockie <- "https://datarockie.com/blog/"

blog_name <- datarockie %>%
  read_html() %>%
  html_elements("h1.entry-title") %>%
  html_text2()
blog_name


blog_cat <- datarockie %>%
  read_html() %>%
  html_elements("span.cat-links") %>%
  html_text2()
blog_cat 

blog_date <- datarockie %>%
  read_html() %>%
  html_elements("time.entry-date.published") %>%
  html_text2()
blog_date

datarockie_blog <- data.frame(blog_name,blog_cat,blog_date)
View(datarockie_blog)


## 2 Homework - crypto price Query
investing_url <- "https://www.investing.com/crypto/currencies"

Name_crypto <- investing_url %>%
  read_html() %>%
  html_elements("td.left.bold.elp.name.cryptoName.first.js-currency-name") %>%
  html_text2()
Name_crypto

Price <- investing_url %>%
  read_html() %>%
  html_elements("td.price.js-currency-price") %>%
  html_text2()
Price


Marketcap <- investing_url %>%
  read_html() %>%
  html_elements("td.js-market-cap") %>%
  html_text2()
Marketcap


Volume24h <- investing_url %>%
  read_html() %>%
  html_elements("td.js-24h-volume") %>%
  html_text2()
Volume24h


Totalvolume <- investing_url %>%
  read_html() %>%
  html_elements("td.js-total-vol") %>%
  html_text2()
Totalvolume

Change24h <- investing_url %>%
  read_html() %>%
  html_elements("td.js-currency-change-24h") %>%
  html_text2()
Change24h


Change7D <- investing_url %>%
  read_html() %>%
  html_elements("td.js-currency-change-7d") %>%
  html_text2()
Change7D





investing_fulldata <- data.frame(Name_crypto, Price, Marketcap, Volume24h, Totalvolume, Change24h, Change7D)

investing_fulldata <- investing_fulldata %>%
  mutate(date = Sys.Date()) %>%
  mutate(date = Sys.time())

View(investing_fulldata)




























