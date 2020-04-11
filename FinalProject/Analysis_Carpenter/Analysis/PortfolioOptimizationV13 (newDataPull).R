# LIBRARY  --------------------------------------------------------------------------------------------------------
  library(tidyverse)
  library(dplyr)
  library(knitr)            # Used to concatenate words
  library(lubridate)        # Used to clean up dates
  library(tidyr)            # Used to pivot data frame
  library(BatchGetSymbols)  # Used to Pull in Stock Data
  library(Rglpk)            # Used to optimize portfolio weights
  library(ggthemes)

# INPUTS  -------------------------------------------------------------------------------------------------------
    startDate    <- Sys.Date() - 365 * 5
    endDate     <- Sys.Date()
    
    #df.SP500     <- GetSP500Stocks()
    #stockList   <- df.SP500$Tickers
    stockList    <- c('JNJ', 'PG', 'JPM', 'FB', 'MSFT', 'AAPL','MMM', 
                    'CL', 'ED', 'MCK', 'DE', 'PFE', 'AXP', 'WU', 'GLW')
    riskFreeRate  <- .0160
    desiredReturn       <- 0.08
    dollarsInvested     <- 10000
  
# DATA PULL -----------------------------------------------------------------------------------------------------
  # Make Variables Monthly
    desiredReturn <- desiredReturn / 12
    riskFreeRate  <- riskFreeRate / 12
    
  # Pull Stock Data
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
    df.return <- as.data.frame(df %>% 
      group_by(stockName) %>%
      summarise(avgMonthlyReturn = mean(stockReturn)))
      rownames(df.return) = df.return$stockName
    df.return <- t(data.matrix(df.return %>%
      select(-stockName)))
    
  # Calculate Excess Returns
    df <- df %>%
      mutate(stockReturn = stockReturn - riskFreeRate) %>%
      rename(excessReturn = stockReturn)
    
    df <- pivot_wider(df, 
                      names_from = stockName, 
                      values_from = excessReturn)
    
  # Create Matrix for Var-Cov Matrix Calculation
    df.matrix     <- data.matrix(df %>% select(-date)) # Converts df to Matrix
    
# MAIN -----------------------------------------------------------------------------------------------------------
  # Create Variance-Covariance Matrix
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

      portfolioOptimal <- cbind(as.data.frame(optimalWeights$solution),
                                as.data.frame(optimalWeights$solution) * dollarsInvested)
        colnames(portfolioOptimal)  <- c("Stock Weight", "Dollar Investment")
        rownames(portfolioOptimal)  <- stockList
      
  # Calculate Risk of Porfolio
      risk            <- sqrt((t(portfolioOptimal$`Stock Weight`) %*% VarCovMatrix) %*% portfolioOptimal$`Stock Weight`) # Standard Deviation (or volatility) of the portfolio, i.e. risk
      
  # Calculate Expected Return of Portfolio
      expectedReturn  <- 12 * (df.return %*% portfolioOptimal$`Stock Weight`)
  
  # Calculate Sharpe Ratio for Portfolio
      sharpeRatio     <- (expectedReturn - (riskFreeRate * 12)) / risk   # objective function <- goal is to maximize return per unit risk taken
      
  # Summary Table
      summaryTable <- as.data.frame(rbind(risk,expectedReturn, sharpeRatio))
        colnames(summaryTable)  <- c("Value")
        rownames(summaryTable)  <- c("Risk", "Expected Return", "Sharpe Ratio")
        
      cat("Total Investment:",dollarsInvested, sep = " ")
      print(portfolioOptimal)
      print(summaryTable)
  
  