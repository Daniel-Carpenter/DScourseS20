library(dbplyr)
library(tidyverse)

df <- tibble(
  y = c(1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0),
  x = c('a', 'a', 'a', 'b', 'b', 'c', 'c'))

print(df)

df <- df %>%
        group_by(x) %>%
        summarise(y = sum(y))
print(df)