# Utility functions used in all the other scripts

library(httr)
library(jsonlite)


query_base <- function(query) {
    return(GET(paste(BASE_URL, query, sep = '')))
}


read_user <- function(file_path) {
    return(fromJSON(readLines(file_path)))
}


vec_to_string <- function(data, sep=' ') {
    string <- data[1]
    for (value in data[-1])
        string = paste(string, toString(value), sep=sep)

    return(string)
}


parse_line <- function(line, sep) {
    parts <- unlist(strsplit(line, sep))
    if (length(parts) > 3) {
        last_part <- ''
        for(part in parts[3:length(parts)])
            last_part = paste(last_part, part, sep='')
        parts = parts[1:3]
        parts[3] = last_part
    }
    return(parts)
}


load_id_content <- function(file, from, to, normalize = TRUE, index = 3) {
    result <- lapply(scan(file, '', skip = from, nlines = to, sep = '\n', quiet = TRUE),
        FUN = function(x) {
            if (normalize == TRUE)
                x = gsub(' ', '_', x)
            out <- parse_line(x, sep = ';')[index]
        }
    )
    return(unlist(result))
}


trim <- function(x) {
    trimmed <- gsub("[\"\\#\\$\\'\\\\~*\\@\\[\\|]|\\s{2,}", ' ', x)
    return(gsub('^\\s+|\\s+$', '', trimmed))
}


':=' <- function(lhs, rhs) {
    "Defines the ':='

    Small 'R hack' that allows you to unpack multiple output variables 
    to separate environment variables
    "
    frame <- parent.frame()
    lhs <- as.list(substitute(lhs))
    if (length(lhs) > 1)
        lhs <- lhs[-1]

    if (length(lhs) == 1) {
        do.call('=', list(lhs[[1]], rhs), envir=frame)
        return(invisible(NULL)) 
    }
    if (is.function(rhs) || is(rhs, 'formula'))
        rhs <- list(rhs)

    if (length(lhs) > length(rhs))
        rhs <- c(rhs, rep(list(NULL), length(lhs) - length(rhs)))

    for (i in 1:length(lhs))
        do.call('=', list(lhs[[i]], rhs[[i]]), envir=frame)

    return(invisible(NULL)) 
}


pop_from_list <- function(x, key, default=NULL) {
    if (is.null(names(x)))
        logic <- x != key
    else
        logic <- names(x) != key
    value <- x[key]
    if (is.null(value))
        value <- default
    return(list(x[logic], unlist(unname(value))))
}


list_to_array <- function(data) {
    arr <- array(0, dim = dim(data))
}


read_config <- function() {
    source('config.R')
    for(name in names(PATHS)) {
        opt <- PATHS[[name]]
        if(!dir.exists(opt)) {
            print(source('Path: ', opt, ' does not exist.'))
        }
    }
}


get_file_indices <- function(path, fixture=NULL) {
    out <- lapply(list.files(path), FUN=function(x) {
            name <- unlist(strsplit(x, '\\.'))[1]
            if(is.null(fixture)) {
                parts <- unlist(strsplit(name, '_'))
                index <- parts[length(parts)]
            }
            else
                index <- unlist(strsplit(name, fixture))[2]
            out <- index
        }
    )
}


get_latest_file_name <- function(option, fixture=NULL) {
    path <- PATHS[[option]]
    files <- list.files(path)
    indices <- get_file_indices(path, fixture)
    if(length(indices) == 0)
        return(NULL)
    names(indices) = files
    latest <- names(indices[order(unlist(indices))])[1]
    return(paste(path, latest, sep = ''))
}


get_path <- function(option, name = NULL) {
    if(is.null(name))
        path <- PATHS[[option]]
    else
        path <- file.path(PATHS[[option]], name)
    if(!is.null(name) && !file.exists(path))
        print('Given path does not exists.')
    return(path)
}


store_model_weights <- function(model) {
    path <- get_path('weights')
    indices <- unlist(get_file_indices(path))
    if(length(indices) == 0)
        index <- 0
    else
        index <- indices[length(indices)]
    filename <- paste('weights_', toString(index), '.h5', sep='')
    weights_path <- file.path(path, filename)
    save_model_hdf5(model, weights_path)
    return(weights_path)
}
