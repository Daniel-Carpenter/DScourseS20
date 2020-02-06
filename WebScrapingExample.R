library(rvest) 

m100 <- read_html("http://en.wikipedia.org/wiki/Men%27s_100_metres_world_record_progression") 

m100 %>%
  html_nodes("#mw-content-text > div > table:nth-child(8)") %>%
  html_table(fill=TRUE)

m100 <- m100[[1]]

#table <- table %>%mutate (ref=NULL)