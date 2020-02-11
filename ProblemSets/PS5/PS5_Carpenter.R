library(tidyverse)
library(rvest)
library(httr)

## INPUTS
  link <- "https://www.macrotrends.net/2324/sp-500-historical-chart-data"
  selector <- "#style-1 > table"

## Link to table in webpage
  list <- read_html(link)
  list %>%
    html_nodes(selector) %>%
    html_table(fill=TRUE) 

## Find Class Type
  table <-
    list %>%
    html_nodes(selector) %>%
    html_table(fill=TRUE) 
  class(table)

## List to data_frame
  table <- 
    table %>%
    bind_rows() %>%
    as_tibble()
  table
  
#################################

## Pull from Yahoo Finance
  
library(rtsdata)
library(dplyr)

startDate <- "1980-01-01"
  
AAPL  <- ds.getSymbol.yahoo('AAPL', from = startDate, to = Sys.Date())
SnP   <- ds.getSymbol.yahoo('^GSPC', from = startDate, to = Sys.Date())
GOOG   <- ds.getSymbol.yahoo('GOOG', from = startDate, to = Sys.Date())
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  