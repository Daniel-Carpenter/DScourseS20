<<<<<<< HEAD
library(tidyverse)
library(rtsdata)
library(skimr)
library(ggthemes)
library(tsibble)
library(dplyr)
library(knitr)      # Used to concatenate words
library(lubridate)

# INPUTS  -----------------------------------------------------------------------------------------------------------
  startDate <- "1999-01-01"
  endDate   <- Sys.Date() 
  stockList <- c('AMZN')

# Pull in Stock Data ------------------------------------------------------------------------------------------------
  df <- as.data.frame(ds.getSymbol.yahoo(stockList[1], from = startDate, to = endDate)) ### Remember that this only includes one var right now
  
  ## Manipualte Dataset ---------------------------------------------------------------------------------------------
    ### Headers
      replacedWord <- combine_words(c(stockList[1], 'Adjusted'), and = ".") # replacedWord = AMZN.Adjusted
      df <- df %>% rename(stockPrice = replacedWord) %>% # note that stockPrice = to the Adjusted Close Price 
        select(stockPrice)
    
    ### Add in Date
      df <- df %>% add_column(stockName = stockList[1], .before = "stockPrice") %>% 
        add_column(dateRename = 0)
        startDate <- as_date(startDate)
        df$dateRename[1] = startDate
      
        tableLength <- NROW(df$stockPrice) - 1
        for (i in 1:tableLength) # fills 'dateRename' column with number that will be replaced by date value
        {
          i = i + 1
          j = i + 1
          
          if (df$stockName[i] != df$stockName[j] & i != tableLength + 1)
          {
            df$date[j] = startDate
          }
          else
          {
            df$dateRename[i] = df$dateRename[i - 1] + 1
          }
        }
        
        df <- df %>% add_column(date = as_date(df$dateRename)) %>%
          select(date,stockName,stockPrice)


      
=======
library(tidyverse)
library(rtsdata)
library(skimr)
library(ggthemes)
library(tsibble)
library(dplyr)
library(knitr)      # Used to concatenate words
library(lubridate)

# INPUTS  -----------------------------------------------------------------------------------------------------------
  startDate <- "1999-01-01"
  endDate   <- Sys.Date() 
  stockList <- c('AMZN')

# Pull in Stock Data ------------------------------------------------------------------------------------------------
  df <- as.data.frame(ds.getSymbol.yahoo(stockList[1], from = startDate, to = endDate)) ### Remember that this only includes one var right now
  
  ## Manipualte Dataset ---------------------------------------------------------------------------------------------
    ### Headers
      replacedWord <- combine_words(c(stockList[1], 'Adjusted'), and = ".") # replacedWord = AMZN.Adjusted
      df <- df %>% rename(stockPrice = replacedWord) %>% # note that stockPrice = to the Adjusted Close Price 
        select(stockPrice)
    
    ### Add in Date
      df <- df %>% add_column(stockName = stockList[1], .before = "stockPrice") %>% 
        add_column(dateRename = 0)
        startDate <- as_date(startDate)
        df$dateRename[1] = startDate
      
        tableLength <- NROW(df$stockPrice) - 1
        for (i in 1:tableLength) # fills 'dateRename' column with number that will be replaced by date value
        {
          i = i + 1
          j = i + 1
          
          if (df$stockName[i] != df$stockName[j] & i != tableLength + 1)
          {
            df$date[j] = startDate
          }
          else
          {
            df$dateRename[i] = df$dateRename[i - 1] + 1
          }
        }
        
        df <- df %>% add_column(date = as_date(df$dateRename)) %>%
          select(date,stockName,stockPrice)


      
>>>>>>> 183901264f97b0fd25842567da93c83ca4fbead6
      