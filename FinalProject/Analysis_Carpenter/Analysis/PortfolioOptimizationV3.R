# LIBRARY  --------------------------------------------------------------------------------------------------------
library(tidyverse)
library(rtsdata)
library(skimr)
library(ggthemes)
library(tsibble)
library(dplyr)
library(knitr)      # Used to concatenate words
library(lubridate)  # Used to clean up dates
library(Stack)      # Used to "Stack", or append data frames
library(rvest)      # Used to web scrape stock list

# INPUTS  ---------------------------------------------------------------------------------------------------------
  startDate           <- "1999-01-01"
  endDate             <- Sys.Date() 
  stockList.Url       <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies#References"
  stockList.Node      <- "#constituents"
  stockList.colNumber <- 1
  
  stockList <- c('AMZN', 'AAPL', 'MSFT')

# FUNCTION LIST ---------------------------------------------------------------------------------------------------
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
  pullData            <- function(startDate, endDate, stockList) # calls 'manipulateStockData' and 'yahooFinance' functions and appends data
  {
    df <- list()
    
    for(i in 1:length(stockList))
    {
      temp <- manipulateStockData(i, startDate,stockList, yahooFinance(i, startDate, endDate, stockList))
      df <- rbind(df,temp) # this appends all stocks together (by row)
    }
    return(df)
    
    #Note:
    ## 1. pivot_wider() <- pivots table, also has ability to rename variables on output 
    ## 2. change rbind to cbind if you want to stack by column
  }
  webData             <- function(url, node, colNumber)
  {
    webTable <- read_html(url) %>%
      html_node(node) %>%
      html_table()
    
    webTable <- as.array(webTable[,colNumber]) # change to array becasuse needs to reference elements

    return(webTable)
  }

# MAIN METHOD -----------------------------------------------------------------------------------------------------
  newList <- webData(stockList.Url,stockList.Node,stockList.colNumber)
  df <- pullData(startDate, endDate, stockList)

  