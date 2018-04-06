# Script to login client to MediaWiki

library(methods)

#if (!exists('query_base', mode = 'function'))
source('src/utils.R')

#TODO add read user.json file for user_name and password

Client <- setRefClass('Client',
    fields = list(
        user_name = 'character',
        password = 'character',
        token = 'character',
        cookies = 'list',
        end_point = 'character'
    ),
    methods = list(
        login = function() {
            token_action <- paste('?action=query&meta=tokens&type=login&format=json',  sep = '')
            print(token_action)
            token_response <- query_base(token_action)

            # Check if the response was successful
            if (token_response$status_code != 200)
                return()

            cookies <<- as.list(token_response$cookies)
            token <<- content(token_response)$query$tokens$logintoken

            login_action <- paste('?action=login', '&lgname=', user_name, '&format=json', sep = '')
            login_body <- list('lgpassword' = password, 'lgtoken' = token)
            print(paste(end_point, login_action, sep = ''))
            login_response <- POST(paste(end_point, login_action, sep=''), body = login_body, set_cookies=cookies)
            print(content(login_response))
        },
        logout = function() {
            #TODO
        },
        do_query = function(query) {
            #TODO
            return('')
        }
    )
)

c <- Client(user_name = user_name, password = password, end_point=BASE_URL)
c$login()
