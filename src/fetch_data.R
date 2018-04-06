# Script to featch data from wikipedia API

library(httr)
library(jsonlite)

query_generator <- function(from, to) {

}


fetch_ids <- function(base_url) {

    request <- paste(base_url, action, sep = '')
    response <- GET(request, )
    print(str(response))
}


data_fetcher <- function() {

    END_POINT = 'https://fi.wikipedia.org/w/api.php'

    #TODO: 
    # check if data already exists
    # Exit if it does
    # otherwise run:

    #LOGIN
    fetch_ids(END_POINT)
    #FETCH DATA WITH IDS
}

action = '?action=query&list=allpages&aplimit=200&apfilterredir=nonredirects&apfrom=0&apto=200&format=json'

data_fetcher()