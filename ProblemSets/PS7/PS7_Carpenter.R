library(readr)
library(stargazer)
library(mice)
library(dplyr)
library(tidyverse)
library(skimr)

# READ IN WAGES FILE FROM MODELING FOLDER
  wages <- read_csv("ModelingOptimization/wages.csv")
  skim(wages)

# DROP NA'S AND CREATE LaTEX CODE
  df <- wages %>% drop_na(hgc,tenure)
  stargazer(df) # LaTEX
  # NA's for logwage  total 560 observations

# REGRESSION ON LOG(WAGES)
  est <- lm(logwage ~ hgc + college + tenure + I(tenure^2) + age + married,data = df)
  summary(est)