library(tidyverse)

mtcars %>% glimpse()

mtcars %>% 
  select(mpg, hp, wt) %>%
  summarise(avg_mpg = mean(mpg))

print("done!")
