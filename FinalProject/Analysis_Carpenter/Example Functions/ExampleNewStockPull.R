library(BatchGetSymbols)
library(tidyverse)
library(lubridate)
library(dplyr)

#df.SP500 <- GetSP500Stocks()
#stockNames    <- df.SP500$Tickers

first.date    <- Sys.Date() - 365 * 5
last.date     <- Sys.Date()
stockNames    <- c('JNJ', 'PG', 'JPM', 'FB', 'JNJ')
riskFreeRate  <- .0160

df <- BatchGetSymbols(tickers = stockNames, 
                      first.date = first.date,
                      last.date = last.date, 
                      freq.data = 'monthly',
                      cache.folder = file.path(tempdir(), 'BGS_Cache'))

df <- as.data.frame(df$df.tickers) %>%
      select(ref.date,
             ticker,
             ret.adjusted.prices) %>%
      mutate(ref.date = as.Date(ref.date)) %>%
      rename(date = ref.date) %>%
      rename(stockName = ticker) %>%
      rename(stockReturn = ret.adjusted.prices) %>%
      drop_na()

df.return <- df %>% 
              group_by(stockName) %>%
              summarise(avgMonthlyReturn = mean(stockReturn)) %>% # Creates table with average return per stock
              mutate(avgAnnualReturn = avgMonthlyReturn * 12)

df <- df %>%
      mutate(stockReturn = stockReturn - riskFreeRate) %>%
      rename(excessReturn = stockReturn)

df <- pivot_wider(df, names_from = stockName, values_from = excessReturn)




