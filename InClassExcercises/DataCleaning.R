library(skimr)
library(tidyverse)
library(huxtable)

df <- as_tibble(mtcars)
skim(df)

cor(df$mpg,df$wt)

exp <- lm(log(mpg) ~ wt,data=df)
tidy(exp)

df1 %>% group_by(gear) %>% summarize()