<<<<<<< HEAD
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
library(tidyr)      # Used to pivot data frame

# INPUTS  ---------------------------------------------------------------------------------------------------------
  startDate           <- "1999-01-01"
  endDate             <- "2020-01-01"
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
        add_column(stockName = stockList[listNum], .before = "stockPrice") %>% 
        select(stockName,stockPrice)
      
      df <- cbind(date=as.Date(rownames(df)),df)
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
  pullWebList         <- function(url, node, colNumber)
  {
    webTable <- read_html(url) %>%
      html_node(node) %>%
      html_table()
    
    webTable <- as.array(webTable[,colNumber]) # change to array becasuse needs to reference elements

    return(webTable)
  }

# MAIN METHOD -----------------------------------------------------------------------------------------------------
  df <- pullData(startDate, endDate, stockList)
  df.pivot <- pivot_wider(df, names_from = stockName, values_from = stockPrice)

=======
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
library(tidyr)      # Used to pivot data frame

# INPUTS  ---------------------------------------------------------------------------------------------------------
  startDate           <- "1999-01-01"
  endDate             <- "2020-01-01"
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
        add_column(stockName = stockList[listNum], .before = "stockPrice") %>% 
        select(stockName,stockPrice)
      
      df <- cbind(date=as.Date(rownames(df)),df)
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
  pullWebList         <- function(url, node, colNumber)
  {
    webTable <- read_html(url) %>%
      html_node(node) %>%
      html_table()
    
    webTable <- as.array(webTable[,colNumber]) # change to array becasuse needs to reference elements

    return(webTable)
  }

# MAIN METHOD -----------------------------------------------------------------------------------------------------
  df <- pullData(startDate, endDate, stockList)
  df.pivot <- pivot_wider(df, names_from = stockName, values_from = stockPrice)

>>>>>>> bee6c0fb5d03b1c9e717f36f610d96d5e325a32f
  