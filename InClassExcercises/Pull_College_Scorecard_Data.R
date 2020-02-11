## Downloading data from College Scorecard


library(tidyverse)
library(rscorecard)
key <- Sys.getenv("GOVT_API_KEY")
sc_key(key)
data <- sc_init() %>%
    sc_select_(c('unitid','instnm','iclevel','ugds')) %>%
    sc_year(2017) %>%
    sc_get()

