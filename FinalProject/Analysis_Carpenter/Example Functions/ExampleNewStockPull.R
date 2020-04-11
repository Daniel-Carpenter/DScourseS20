library(BatchGetSymbols)
library(tidyverse)
library(lubridate)
library(dplyr)

# INPUTS  -------------------------------------------------------------------------------------------------------
  startDate    <- Sys.Date() - 365 * 5
  endDate     <- Sys.Date()
  #df.SP500     <- GetSP500Stocks()
  #stockNames   <- df.SP500$Tickers
  stockList    <- c('JNJ', 'PG', 'JPM', 'FB', 'JNJ')
  riskFreeRate  <- .0160

# PULL IN STOCK DATA --------------------------------------------------------------------------------------------
    df <- BatchGetSymbols(tickers = stockList, 
                          first.date = startDate,
                          last.date = endDate, 
                          freq.data = 'monthly',
                          cache.folder = file.path(tempdir(), 'BGS_Cache'))
    
  # Mutate Data (Drop Cols and Rename)
    df <- as.data.frame(df$df.tickers) %>%
          select(ref.date,
                 ticker,
                 ret.adjusted.prices) %>%
          mutate(ref.date = as.Date(ref.date)) %>%
          rename(date = ref.date) %>%
          rename(stockName = ticker) %>%
          rename(stockReturn = ret.adjusted.prices) %>%
          drop_na()
    
  # Create Avg. Return Table (by Stock)
    df.return <- df %>% 
                  group_by(stockName) %>%
                  summarise(avgMonthlyReturn = mean(stockReturn)) %>%
                  mutate(avgAnnualReturn = avgMonthlyReturn * 12)

  
  # Calculate Excess Returns
    df <- df %>%
          mutate(stockReturn = stockReturn - riskFreeRate) %>%
          rename(excessReturn = stockReturn)
    
    df <- pivot_wider(df, 
                      names_from = stockName, 
                      values_from = excessReturn)

