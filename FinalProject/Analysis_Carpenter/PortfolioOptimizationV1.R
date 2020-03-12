library(tidyverse)
library(rtsdata)
library(dplyr)
library(skimr)
library(ggthemes)
library(tsibble)


startDate <- "1999-01-01"
endDate   <- Sys.Date() 

stockList   <- c('^GSPC', 'AAPL', 'AMZN')
maxStockNum <- NROW(stockList)

for (i in 1:maxStockNum)
{
    df[i] <- ds.getSymbol.yahoo(stockList[i], from = startDate, to = endDate)
}


