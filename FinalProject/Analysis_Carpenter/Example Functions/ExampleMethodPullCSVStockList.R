library(readr)
library(tidyverse)


SnP_CSV.colNumber <- 1

SnP_CSV <- as.data.frame(read_csv("FinalProject/Analysis_Carpenter/Data Import/constituents-financials_csv.csv"))

SnP_CSV <- as.array(SnP_CSV[,SnP_CSV.colNumber])

