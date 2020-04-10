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
  library(Rglpk)      # Used to optimize portfolio weights

# INPUTS  -------------------------------------------------------------------------------------------------------
    startDate           <- "2015-01-01"
    endDate             <- "2020-04-01"
    stockList           <- c('JNJ', 'PG', 'JPM') # , 'FB', 'JNJ', 'GOOG', 'GOOGL', 'PG', 'JPM'
    desiredReturn       <- .02
    dollarsInvested     <- 10000
    
    ##Used for Stock Webscape
      #stockList.Url       <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies#References"
      #stockList.Node      <- "#constituents"
      #stockList.colNumber <- 1
  
# FUNCTION LIST -------------------------------------------------------------------------------------------------
    manipulateStockData <- function(listNum, startDate, stockList)  # groups stockPrice by year and month 
    {
      ## Manipualte Dataset ---------------------------------------------------------------------------------------------
        df <- as.data.frame(ds.getSymbol.yahoo(stockList[listNum], from = startDate, to = endDate))
      
        replacedWord <- combine_words(c(stockList[listNum], 'Adjusted'), and = ".") # replacedWord = AMZN.Adjusted
        df <- df %>% rename(stockPrice = replacedWord) %>% # note that stockPrice = to the Adjusted Close Price 
          add_column(stockName = stockList[listNum], .before = "stockPrice") %>% 
          select(stockName,stockPrice)
        
        df <- cbind(date = as.Date(rownames(df)),df)
        df <- cbind(monthNum = month(as.Date(rownames(df))),df)
        df <- cbind(year = year(as.Date(rownames(df))),df)
        df <- as.data.frame(df %>% 
                group_by(year, monthNum, stockName) %>%
                summarise(stockPrice = sum(stockPrice)))
  
  
        return(df);
    }
    riskFreeRate        <- function(startDate, endDate)
    {
      df <- as.data.frame(ds.getSymbol.yahoo('IRX', from = startDate, to = endDate))
      df <- cbind(date = as.Date(rownames(df)),df)
      df <- cbind(monthNum = month(as.Date(rownames(df))),df)
      df <- cbind(year = year(as.Date(rownames(df))),df)
      df <- as.data.frame(df %>% 
                            group_by(year, monthNum, IRX.Adjusted) %>%
                            summarise(stockPrice = sum(IRX.Adjusted))) %>%
                            mutate(return = IRX.Adjusted / lag(IRX.Adjusted) - 1) %>%
                            select(return) %>%
                            drop_na(return)
      
      riskFreeRate <- mean(df$return) # average t-bill rate used as proxy for risk free rate
      
      return(riskFreeRate)
    }
    avgReturnTable      <- function(startDate, endDate, stockList) # calls 'manipulateStockData', creates table with average monthly return per stock
    {
      # Append Each Stock by Row -------------------------------------------------------------------------------------
      df <- list()
      
      for(i in 1:length(stockList))
      {
        temp  <- manipulateStockData(i, startDate,stockList)
        df    <- rbind(df,temp) # this appends all stocks together (by row)
      }
      
      # Calculate Excess Return and Remove Calculation Columns -------------------------------------------------------
      df <- df %>%  mutate(return = stockPrice / lag(stockPrice) - 1) %>%
        select(-stockPrice) %>%
        drop_na(return) %>% 
        group_by(stockName) %>%
        summarise(avgMonthlyReturn = mean(return)) # Creates table with average return per stock
      
      # Pivot Data   -------------------------------------------------------------------------------------------------
      df <- as.data.frame(pivot_wider(df, names_from = stockName, values_from = avgMonthlyReturn) %>%
                            drop_na) ## drops rows for when firms were not publically traded
      rownames(df)  <- c("avgMonthlyReturn")
      
      df <- t(data.matrix(df))
      
      
      return(df)
    }
    pullData            <- function(startDate, endDate, stockList) # calls 'manipulateStockData', Pulls in T-Bill data, appends, then pivots data
    {
      # Pull in T-Bill Data as Proxy for Risk Free Rate -------------------------------------------------------------
        riskFreeRate <- riskFreeRate(startDate, endDate) # average t-bill rate used as proxy for risk free rate
      
      # Append Each Stock by Row -------------------------------------------------------------------------------------
        df <- list()
        
        for(i in 1:length(stockList))
        {
          temp  <- manipulateStockData(i, startDate,stockList)
          df    <- rbind(df,temp) # this appends all stocks together (by row)
        }
        
      # Calculate Excess Return and Remove Calculation Columns -------------------------------------------------------
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
  df            <- pullData(startDate, endDate, stockList)
  df.return     <- avgReturnTable(startDate, endDate, stockList)
  riskFreeRate  <- riskFreeRate(startDate, endDate)
    
  
  # Create Variance-Covariance Matrix
      df.matrix     <- data.matrix(df %>% # Converts df to Matrix
                        select(-monthNum, -year))
  
      VarCovMatrix  <- (t(df.matrix) %*% df.matrix) / length(df) # Creates Variance Covariance Matrix
  
  # Create Weights Table
      stockWeights <- c()

      for (i in 1:length(stockList))
      {
        stockWeights[i] <- 1
      }
      stockWeights              <- as.data.frame(stockWeights)
        colnames(stockWeights)  <- c("portfolioWeight")
        rownames(stockWeights)  <- stockList
        
      stockWeights <- t(as.matrix(stockWeights))
      stockWeights1 <- stockWeights
      
  # Optimize Risk, Given desired Expected Return
      #objectiveFun      <- sqrt(diag(VarCovMatrix)) 
      tempVarCovMatVector <- sqrt(stockWeights1 %*% VarCovMatrix)
      
      objectiveFun      <- tempVarCovMatVector
      constraintInputs  <- rbind(stockWeights, as.vector(df.return))
      direction         <- c("==", "==")
      constraintValues  <- c(1, desiredReturn)
      
      optimalWeights    <- Rglpk_solve_LP(objectiveFun,
                                          constraintInputs,
                                          direction,
                                          constraintValues,
                                          max = FALSE)
      objectiveFun
      df.return
      
      
      
      portfolioOptimal <- cbind(as.data.frame(optimalWeights$solution),
                                as.data.frame(optimalWeights$solution) * dollarsInvested)
        colnames(portfolioOptimal)  <- c("Stock Weight", "Dollar Investment")
        rownames(portfolioOptimal)  <- stockList
      
  # Calculate Risk of Porfolio
      risk            <- sqrt((t(portfolioOptimal$`Stock Weight`) %*% VarCovMatrix) %*% portfolioOptimal$`Stock Weight`) # Standard Deviation (or volatility) of the portfolio, i.e. risk
      
  # Calculate Expected Return of Portfolio
      expectedReturn  <- t(df.return) %*% portfolioOptimal$`Stock Weight`
  
  # Calculate Sharpe Ratio for Portfolio
      sharpeRatio     <- (expectedReturn - riskFreeRate) / risk   # objective function <- goal is to maximize return per unit risk taken
      
  # Summary Table
      summaryTable <- as.data.frame(rbind(risk,expectedReturn, sharpeRatio))
        colnames(summaryTable)  <- c("Value")
        rownames(summaryTable)  <- c("Risk", "Expected Return", "Sharpe Ratio")
        
      cat("Total Investment:",dollarsInvested, sep = " ")
      print(portfolioOptimal)
      print(summaryTable)
  
  