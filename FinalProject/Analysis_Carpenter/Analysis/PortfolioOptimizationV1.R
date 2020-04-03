library(tidyverse)
library(rtsdata)
library(dplyr)
library(skimr)
library(ggthemes)
library(tsibble)


startDate <- "1999-01-01"
endDate   <- Sys.Date() 

stockList <- c('^GSPC', 'AMZN')
df <- ds.getSymbol.yahoo(stockList[1], from = startDate, to = endDate)

for (i in 1:NROW(stockList))
{
    i <- append(stockData,ds.getSymbol.yahoo(stockList[i], from = startDate, to = endDate))
    ++i
}

df <- as.data.frame(df)

