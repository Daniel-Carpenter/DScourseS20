library(tidyverse)
library(rtsdata)
library(dplyr)
library(skimr)
library(ggthemes)

startDate <- "1999-01-01"

#Pull in S&P 500, Apple, and Google Stock; Store as Data Fram to add index column
  SnP  <- ds.getSymbol.yahoo('^GSPC', from = startDate, to = Sys.Date())
  SnP  <- as.data.frame(SnP) %>% mutate(index = row_number())

  AAPL <- ds.getSymbol.yahoo('AAPL', from = startDate, to = Sys.Date())
  AAPL <- as.data.frame(AAPL) %>% mutate(index = row_number())

  AMZN <- ds.getSymbol.yahoo('AMZN', from = startDate, to = Sys.Date())
  AMZN <- as.data.frame(AMZN) %>% mutate(index = row_number())  

#Make Sure History Aligns (i.e. were all companies publically traded at 'startDate')
  skim(SnP)
  skim(AAPL)
  skim(AMZN)

#Combine data sets and select only adjusted closes
  portfolio  <- merge(SnP, AAPL, by = "index")
  portfolio  <- merge(portfolio, AMZN, by = "index")
  portfolio  <- portfolio %>% select(index, 
                      GSPC.Adjusted, 
                      AAPL.Adjusted,
                      AMZN.Adjusted)
#Rename Columns
  portfolio  <- portfolio %>% rename(GSPC = GSPC.Adjusted, 
                                     AAPL = AAPL.Adjusted,
                                     AMZN = AMZN.Adjusted)

# Plot Data
  
  #S&P 500
    ggplot(data = portfolio, aes(
      x = index, 
      y = GSPC,
      color = GSPC)) +
      geom_line() +
      theme_minimal() +
      labs(title = "S&P 500 Stock Price over 21-Years",
           subtitle = "(1999-2020)",
           caption = "Data from Yahoo Finance.",
           tag = "Figure 1",
           x = "Date Since Jan. 1 1999, Indexed",
           y = "S&P 500 Adj. Stock Price")
  
  
  #Apple
    ggplot(data = portfolio, aes(
      x = index, 
      y = AAPL,
      color = AAPL)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Apple Stock Price over 21-Years",
           subtitle = "(1999-2020)",
           caption = "Data from Yahoo Finance.",
           tag = "Figure 2",
           x = "Date Since Jan. 1 1999, Indexed",
           y = "Apple Adj. Stock Price")
  
    #Amazon
    ggplot(data = portfolio, aes(
      x = index, 
      y = AMZN,
      color = AMZN)) +
      geom_line() +
      theme_minimal() +
      labs(title = "Amazon Stock Price over 21-Years",
           subtitle = "(1999-2020)",
           caption = "Data from Yahoo Finance.",
           tag = "Figure 3",
           x = "Date Since Jan. 1 1999, Indexed",
           y = "Amazon Adj. Stock Price")
    