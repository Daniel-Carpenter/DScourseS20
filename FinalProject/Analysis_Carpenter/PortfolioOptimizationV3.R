library(tidyverse)
library(rtsdata)
library(skimr)
library(ggthemes)
library(tsibble)
library(dplyr)
library(knitr)      # Used to concatenate words
library(lubridate)  # Used to clean up dates
library(Stack)      # Used to "Stack", or append data frames

# INPUTS  ---------------------------------------------------------------------------------------------------------
  startDate <- "1999-01-01"
  endDate   <- Sys.Date() 
  stockList <- c('AMZN', 'AAPL', 'MSFT')

# METHOD LIST (FUNCTIONS)  ----------------------------------------------------------------------------------------

  manipulateStockData <- function(listNum, startDate, stockList, df)  
  {
    ## Manipualte Dataset ---------------------------------------------------------------------------------------------
    ### Headers
    replacedWord <- combine_words(c(stockList[listNum], 'Adjusted'), and = ".") # replacedWord = AMZN.Adjusted
    df <- df %>% rename(stockPrice = replacedWord) %>% # note that stockPrice = to the Adjusted Close Price 
      select(stockPrice)
    
    ### Add in Date
    df <- df %>% add_column(stockName = stockList[listNum], .before = "stockPrice") %>% 
      add_column(dateRename = 0)
    startDate <- as_date(startDate)
    df$dateRename[listNum] = startDate
    
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
    return(df);
  }
  yahooFinance        <- function(listNum, startDate, endDate, stockList)
  {
    df <- as.data.frame(ds.getSymbol.yahoo(stockList[listNum], from = startDate, to = endDate))
    return(df)
  }
  pullData            <- function(listNum, startDate, endDate, stockList) # combines 'manipulateStockData' and 'yahooFinance' functions
  {
    df <- manipulateStockData(listNum, startDate,stockList, yahooFinance(listNum, startDate, endDate, stockList))
    return(df)
  }

# PULL IN DATA ----------------------------------------------------------------------------------------------------
  ## Assign Variables from 'stockList'
    
    for(i in 1:length(stockList))
    {
          assign(paste("st", i, sep = ""), pullData(i, startDate, endDate, stockList))
    }
    
    df <- rbind(st1, st2, st3) ## THIS IS WHERE YOU HAVE TO INPUT VALUES - NEED AUTOMATED FIX

      