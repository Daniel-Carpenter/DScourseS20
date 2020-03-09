library(readr)
library(stargazer)
library(mice)
library(dplyr)
library(tidyverse)
library(skimr)

# READ IN WAGES FILE FROM MODELING FOLDER
  df <- read_csv("ModelingOptimization/wages.csv")
  skim(wages)
  stargazer(df) # LaTEX
  
# DROP NA'S AND CREATE LaTEX CODE
  df <- df %>% drop_na(hgc,tenure)
  # NA's for logwage  total 560 observations
  # Likely missing at random because it could be related to other results

  
# LISTWISE DELETION
  df.listwise <- df %>% drop_na(logwage)
  est.listwise <- lm(logwage ~ hgc + college + tenure + 
       I(tenure^2) + age + married, data = df.listwise)
  summary(est.listwise)
  
# MEAN IMPUTATION
  meanOfDF <- mean(df.listwise$logwage)
  meanOfDF
  df.mean <- df
  df.mean[is.na(df.mean)] <- meanOfDF
  est.mean <- lm(logwage ~ hgc + college + tenure + 
                       I(tenure^2) + age + married, data = df.mean)
  summary(est.mean)
  
# MULTIPLE IMPUTATION
  df.imp = mice(df, seed = 12345)
  summary(df.imp)
  fit = with(df.imp, lm(logwage ~ hgc + college + tenure + 
                          I(tenure^2) + age + married))
  #round(summary(pool(fit)),2)

# COMBINE REGRESSION ESTIMATES
  stargazer(est.listwise, est.mean, title="Regression Results from Finding Missing Values", align=TRUE)
  
  