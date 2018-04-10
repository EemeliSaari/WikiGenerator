# Script to featch data from wikipedia API

library(httr)
library(jsonlite)

source('src/client.R')


fetch_ids <- function(client, id_file) {
    "Fetches all the IDs and page tittles to a .csv file"

    meta_file <- 'data/id_meta.json'
    if (!file.exists(meta_file)) {
        meta <- list(apfrom = '', complete = FALSE)
    }
    else {
        meta <- fromJSON(readLines(meta_file))
    }

    if (meta$complete == TRUE) {
        return(NULL)
    }

    if (!file.exists(id_file)) {
        header <- 'ns;pageid;title'
        write(header, id_file)
    }

    # Query template for allpages
    query <- list(
        action = 'query',
        params = list(
            list = 'allpages',
            aplimit = '500',
            filterredir = 'nonredirects',
            apnamespace = '0',
            apfrom = meta$apfrom,
            format = 'json'
        )
    )
    repeat {
        write(toJSON(meta), meta_file)
        result <- client$do_query(query)

        # Check the query status -> sleep if fails
        if (result$status == FALSE) {
            Sys.sleep(10)
            next
        }

        meta$apfrom = content(result$body)$continue$apcontinue
        if (is.null(content(result$body)$continue$apcontinue)) {
            meta$complete = TRUE
            write(toJSON(meta), meta_file)
            break
        }
        else {
            query$params$apfrom = meta$apfrom
            for (page in content(result$body)$query$allpages) {
                write(vec_to_string(page, sep=';'), id_file, append = TRUE)
            }
        }
        print(content(result$body)$continue$apcontinue)
    }
}


fetch_data <- function(client, id_file, target_folder, chunk_size = 50) {
    "Fetches all the data using IDs"

    dir.create(target_folder, showWarnings = FALSE)

    print('asd')

    meta_file <- 'data/data_meta.json'
    if (!file.exists(meta_file)) {
        meta <- list(from = 1, complete = FALSE)
    }
    else {
        meta <- fromJSON(readLines(meta_file))
    }

    if (meta$complete == TRUE) {
        return(NULL)
    }

    ids <- vec_to_string(load_id_content(id_file, from = meta$from, to = chunk_size, index = 1), sep = '|')

    query <- list(
        action = 'query',
        params = list(
            prop = 'revisions',
            rvprop = 'content',
            format = 'json',
            pageids = ids
        )
    )
    repeat {
        result <- client$do_query(query, redirects = TRUE)
        # Check the query status -> sleep if fails
        if (result$status == FALSE) {
            Sys.sleep(10)
            next
        }
        for (page in content(result$body)$query$pages) {
            name <- paste(page$pageid, '.txt', sep = '')
            file_path <- file.path(target_folder, name)
            cont <- unlist(page$revisions[[1]]['*'])
            if (!file.exists(file_path)) {
                write(cont, file_path)
            }
        }
        meta$from = meta$from + chunk_size
        print(meta$from)
        query$params$pageids = vec_to_string(load_id_content(id_file, from = meta$from, to = chunk_size, index = 1), sep = '|')
        if (identical(ids, character(0))) {
            meta$status = TRUE
            write(toJSON(meta), meta_file)
            break
        }

        write(toJSON(meta), meta_file)
    }
}


fetcher <- function(data_paths) {
    "Fetches the article ID's and then the articles with the ID's. Reads a data_paths variable in format list
    [id = id_path, data = articles_path, user = user_path]."

    user_data <- read_user(data_paths$user)

    client <- Client(user_name = user_data$name, password = user_data$password, end_point=BASE_URL)
    client$login()

    fetch_ids(client, data_paths$id)
    fetch_data(client, data_paths$id, data_paths$data)

    client$logout()

    out <- NULL
}


fetcher(list(id = 'data/ids.csv', data = 'M:/Projects/WikiGenerator/data/articles/', user = 'data/user_data.json'))