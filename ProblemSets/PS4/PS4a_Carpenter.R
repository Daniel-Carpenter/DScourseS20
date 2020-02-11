library(jsonlite)

nfl <- fromJSON("http://api.fantasy.nfl.com/v1/players/stats?statType=seasonStats&season=2010&week=1&format=json")
class(nfl)