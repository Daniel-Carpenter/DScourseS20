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


# INPUTS  -------------------------------------------------------------------------------------------------------
  startDate           <- "1990-01-01"
  endDate             <- Sys.Date()
  stockList <- c('GOOG', 'GOLD', 'AMZN') # , 'FB', 'JNJ', 'GOOG', 'GOOGL', 'PG', 'JPM'
  
  ##Used for Stock Webscape
    #stockList.Url       <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies#References"
    #stockList.Node      <- "#constituents"
    #stockList.colNumber <- 1
  
# FUNCTION LIST -------------------------------------------------------------------------------------------------
  manipulateStockData <- function(listNum, startDate, stockList)  # potentially consolidate into 'pullData'
  {
    ## Manipualte Dataset ---------------------------------------------------------------------------------------------
      df <- as.data.frame(ds.getSymbol.yahoo(stockList[listNum], from = startDate, to = endDate))
    
      replacedWord <- combine_words(c(stockList[listNum], 'Adjusted'), and = ".") # replacedWord = AMZN.Adjusted
      df <- df %>% rename(stockPrice = replacedWord) %>% # note that stockPrice = to the Adjusted Close Price 
        add_column(stockName = stockList[listNum], .before = "stockPrice") %>% 
        select(stockName,stockPrice)
      
      df <- cbind(date = as.Date(rownames(df)),df)
      df <- cbind(monthNum = month(as.Date(rownames(df))),df)
      return(df);
  }
  pullData            <- function(startDate, endDate, stockList) # calls 'manipulateStockData', Pulls in T-Bill data, appends, then pivots data
  {
    # Pull in T-Bill Data as Proxy for Risk Free Rate -------------------------------------------------------------
      riskFreeTable            <- as.data.frame(ds.getSymbol.yahoo('IRX', from = startDate, to = endDate)) %>%
        mutate(return = IRX.Adjusted / lag(IRX.Adjusted) - 1) %>%
        select(return) %>%
        drop_na(return)
      riskFreeRate <- mean(riskFreeTable$return)
    
    # Append Each Stock by Row -------------------------------------------------------------------------------------
      df <- list()
      
      for(i in 1:length(stockList))
      {
        temp <- manipulateStockData(i, startDate,stockList)
        df <- rbind(df,temp) # this appends all stocks together (by row)
      }
      
    # Calculate Excess Return and Remoce Calculation Columns -------------------------------------------------------
      df <- df %>% mutate(return = stockPrice / lag(stockPrice) - 1) %>%
        select(-stockPrice) %>%
        drop_na(return) %>%
        mutate(excessReturn = return - riskFreeRate) %>%
        select(-return)

    
    # Pivot Data   -------------------------------------------------------------------------------------------------
      df <- pivot_wider(df, names_from = stockName, values_from = excessReturn) %>%
        drop_na() ## drops rows for when firms were not publically traded

    return(df)
  }
  pullWebList         <- function(url, node, colNumber) # Currently unused
  {
    webTable <- read_html(url) %>%
      html_node(node) %>%
      html_table()
    
    webTable <- as.array(webTable[,colNumber]) # change to array becasuse needs to reference elements

    return(webTable)
  }

# MAIN-----------------------------------------------------------------------------------------------------------
  df <- pullData(startDate, endDate, stockList)
  
  # Create Variance-Covariance Matrix
    df.matrix     <- data.matrix(df) %>% # Converts df to Matrix
      select(-date, -monthNum)

    VarCovMatrix  <- (t(df.matrix) %*% df.matrix) / length(df) # Creates Variance Covariance Matrix
    
  # Create Weights Table
    stockWeights      <- c()
    setInitialWeight  <- 1 / length(stockList)
    
    for (i in 1:length(stockList))
    {
      stockWeights[i]       <- setInitialWeight ### Set at equal weights for initialization
    }
    stockWeights            <- as.data.frame(stockWeights)
    colnames(stockWeights)  <- c("portfolioWeight")
    rownames(stockWeights)  <- stockList
    
    sumOfWeights            <- sum(stockWeights$portfolioWeight) ### Must equal 1.00
    stockWeights            <- as.matrix(stockWeights)
    
  # Calculate Risk of Porfolio
    risk <- sum(sqrt((t(stockWeights) %*% VarCovMatrix) %*% stockWeights))
    
  # Calculate Expected Return of Portfolio
    returnTable <- c()
    
    #for (i in 2:length(stockList))
    #{
      returnTable[1] <- mean(df$GOOG)
    #}
    #returnTable <- as.data.frame(returnTable)
    #colnames(returnTable) <- c("stockReturns")
    #rownames(returnTable) <- stockList
      
   # df <- cbind(month(as.Date(rownames(df))),df)
    
      # Calculate Sharpe Ratio for Portfolio
    
      
    
  