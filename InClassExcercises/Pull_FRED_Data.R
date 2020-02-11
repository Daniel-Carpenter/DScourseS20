## Example of pulling in data from FRED

library(httr)

endpoint = "series/observations"
params = list(
  api_key= "41259f2f42d3ea0fa13aecf6e66b7f34", ## Change to your own key
  file_type="json", 
  series_id="GNPCA"
)

fred <-
  httr::GET(
    url = "https://api.stlouisfed.org/", ## Base URL
    path = paste0("fred/", endpoint), ## The API endpoint
    query = params ## Our parameter list
  )

