# Script to login client to MediaWiki

library(methods)
library(httr)


Client <- setRefClass('Client',
    fields = list(
        user_name = 'character',
        password = 'character',
        token = 'character',
        cookies = 'list',
        lguserid = 'numeric',
        lgusername = 'character',
        end_point = 'character'
    ),
    methods = list(
        login = function(verbose=FALSE) {
            "Login method to login the user. Returns FALSE if the login fails, TRUE if success."

            token_action <- list(action = 'query', params = list(meta = 'tokens', type = 'login', format = 'json'))
            token_response <- do_query(token_action)

            # Check if the response was successful
            if (token_response$status == FALSE)
                return(FALSE)

            cookies <<- as.list(token_response$body$cookies)
            token <<- content(token_response$body)$query$tokens$logintoken

            login_action <- paste('?action=login', '&lgname=', user_name, '&format=json', sep = '')
            login_body <- list('lgpassword' = password, 'lgtoken' = token)

            login_response <- POST(paste(end_point, login_action, sep=''), body = login_body, set_cookies=cookies)
            login_result <- content(login_response)

            # The POST will cause error if client is already logged in
            if ('Aborted' %in% login_result$login$result)
                logout()

            if (verbose == TRUE)
                print(login_result)

            if (login_result$login$result == "Success") {
                return(list(status=TRUE))
            }
            else {
                return(list(status=FALSE))
            }
        },
        logout = function() {
            do_query(list(action = 'logout', params = list(format = 'json')))
        },
        do_query = function(query, redirects=FALSE, verbose=FALSE) {
            "Queries the end_point with query. Query must be in list format: action='', params = list(p1='',p2=''...)"

            query_body <- paste('?action=', query$action, sep = '')

            if (redirects == TRUE) {
                query_body = paste(query_body, '&redirects&', sep = '')
            }

            for (param in names(query$params)) {
                value <- unlist(unname(query$params[param]))
                query_body <- paste(query_body, '&', param, '=', value, sep='')
            }
            url <- paste(end_point, query_body, sep = '')
            result <- GET(url)

            if (verbose == TRUE)
                print(content(result))

            out <- list(status = TRUE, body = result)
            if (result$status_code != 200) {
                out$status = FALSE
                out$body = toString(result$status_code)
            }
            else if('error' %in% names(content(result))) {
                out$status = FALSE
                out$body = content(result)$error$code
            }
            return(out)
        }
    )
)


test_login <- function() {
    "Test a simple login scenario"

    user_data <- read_user('data/user_data.json')

    client <- Client(user_name = user_data$name, password = user_data$password, end_point=BASE_URL)
    login_result <- client$login()
    if (login_result$status == FALSE)
        return('LogginError')

    logout_result <- client$logout()
    if (logout_result$status == FALSE)
        return('LogoutError')

    return('Passed')
}


test_query <- function() {
    "Test the query method"

    user_data <- read_user('data/user_data.json')

    query <- list(action = 'query', params = list(meta = 'tokens', type = 'login', format = 'json'))
    client <- Client(user_name = user_data$name, password = user_data$password, end_point=BASE_URL)
    result <- client$do_query(query)
    if (result$status == FALSE)
        return('Failed')
    else
        return('Passed')
}
