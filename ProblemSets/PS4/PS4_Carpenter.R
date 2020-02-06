library(jsonlite)

mydf <- fromJSON("nflstats.json")
class(mydf$players)

## Object: data.frame

mydf$players[1:10,]

