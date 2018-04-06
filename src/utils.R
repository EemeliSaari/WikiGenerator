# Utility functions used in all the other scripts

library(httr)
library(jsonlite)

BASE_URL <- 'https://fi.wikipedia.org/w/api.php'

query_base <- function(query) {
    return(GET(paste(BASE_URL, query, sep = '')))
}
