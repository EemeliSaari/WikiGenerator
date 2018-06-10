# Utility functions used in all the other scripts

library(httr)
library(jsonlite)


BASE_URL <- 'https://fi.wikipedia.org/w/api.php'


query_base <- function(query) {
    return(GET(paste(BASE_URL, query, sep = '')))
}


read_user <- function(file_path) {
    return(fromJSON(readLines(file_path)))
}


vec_to_string <- function(data, sep=' ') {
    string <- data[1]
    for (value in data[-1]) {
        string = paste(string, toString(value), sep=sep)
    }
    return(string)
}


parse_line <- function(line, sep) {
    parts <- unlist(strsplit(line, sep))
    if (length(parts) > 3) {
        last_part <- ''
        for(part in parts[3:length(parts)]) {
            last_part = paste(last_part, part, sep='')
        }
        parts = parts[1:3]
        parts[3] = last_part
    }
    return(parts)
}


load_id_content <- function(file, from, to, normalize = TRUE, index = 3) {
    return(unlist(lapply(scan(file, '', skip = from, nlines = to, sep = '\n', quiet = TRUE),
                FUN = function(x) {
                    if (normalize == TRUE)
                        x = gsub(' ', '_', x)
                    out <- parse_line(x, sep = ';')[index]
                }
            )
        )
    )
}
