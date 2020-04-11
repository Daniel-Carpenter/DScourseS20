library(quantmod)
library(tidyverse)

startDate           <- "2000-01-01"
endDate             <- "2015-01-01"


data.env <- new.env()
getSymbols("YHOO", env=data.env)
ls.str(data.env)



df1 <- as.xts(getSymbols('DGS3MO', src = "FRED"))
df1 <- data.frame(date=index(df1), coredata(df1))

  df <- as.data.frame(getSymbols('DGS3MO', src = "FRED"))
  df <- cbind(date = as.Date(rownames(df)),df)
  df <- cbind(monthNum = month(as.Date(rownames(df))),df)
  df <- cbind(year = year(as.Date(rownames(df))),df)
  df <- as.data.frame(df %>% 
                        group_by(year, monthNum) %>%
                        summarise(stockPrice = mean(IRX.Adjusted))) %>%
                        drop_na()
    #mutate(return = stockPrice / lag(stockPrice) - 1) %>%
    #select(return) %>%
    #drop_na(return)