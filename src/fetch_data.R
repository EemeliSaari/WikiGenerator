# Script to featch data from wikipedia API

library(httr)
library(jsonlite)

source('src/client.R')


fetch_ids <- function(client, id_file) {
    
    meta_file <- 'data/id_meta.json'
    if (!file.exists(meta_file)) {
        meta <- list(apfrom = '', complete = FALSE)
    }
    else {
        meta <- fromJSON(readLines(meta_file))
    }

    if (!file.exists(id_file)) {
        header <- 'ns;pageid;title'
        write(header, id_file)
    }
    
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
    i <- 0
    repeat {
        write(toJSON(meta), meta_file)
        result <- client$do_query(query)
        meta$apfrom = content(result$body)$continue$apcontinue
        if (content(result$body)$continue$apcontinue %in% c(NULL, '')) {
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
        
        if (i == 3)
          break
        
        i = i + 1
    }
}


fetch_data <- function(client, id_file, target_folder, chunk_size = 200) {
    print('asd')

    titles <- vec_to_string(load_titles(id_file, from=1, to=200), sep='|')
    print(titles)

    query <- list(
        action = 'query',
        params = list(
            titles = titles,
            prop = 'revisions',
            rvprop = 'content',
            format = 'json'
        )
    )
}


fetcher <- function(data_paths) {
  "Fetches the article ID's and then the articles with the ID's. Reads a data_paths variable in format list
  [id = id_path, data = articles_path, user = user_path]."

    user_data <- read_user(data_paths$user)

    client <- Client(user_name = user_data$name, password = user_data$password, end_point=BASE_URL)
    client$login()

    #fetch_ids(client, data_paths$id)
    fetch_data(client, data_paths$id, data_paths$data)

    client$logout()

    out <- NULL
}


fetcher(list(id = 'data/ids.csv', data = 'data/articles/', user = 'data/user_data.json'))