# Scripts to parse Wikitext


for (script in c('wikiparser', 'utils')) {
    source(paste('src/', script, '.R', sep = '', collapse = ''))
}


parts_to_files <- function(parts, path) {
    if (file.exists(path)) {
        return(2)
    }
    file.create(path)
    lapply(parts, FUN = function(x) {
            e <- new.env()
            e$status <- FALSE
            lapply(x, FUN = function(y) {
                    string <- trim(y)
                    if (!string == "") {
                        write(string, file = path, append = TRUE)
                        e$status = TRUE
                    }
                }
            )
            if (e$status) {
                write("", file = path, append = TRUE)
            }
        }
    )
    return(1)
}


parse_files <- function(folder, target, verbose = FALSE) {
    "Parse all the wikitext files from a folder."
    if (!file.exists(folder)) {
        print(paste('ERROR path: ', folder, ' does not exists.'))
        return(NULL)
    }
    if (!file.exists(target) && !dir.create(target)) {
        print(paste('ERROR target path ', target, ' is invalid.'))
        return(NULL)
    }

    parser <- WikiParser(verbose = verbose)

    results <- lapply(list.files(folder), FUN = function(x) {
            file_path <- file.path(folder, x)
            target_path <- file.path(target, x)
            parsed <- parse_text(parser, unlist(readLines(file_path)))
            parts_to_files(parsed, target_path)
        }
    )
    print(results)
    return(NULL)
}


test_parse <- function() {
    example_data <- unlist(readLines('data/articles/926575.txt'))

    parser <- WikiParser(verbose = TRUE)

    start <- Sys.time()
    parts <- parse_text(parser, example_data)
    print(Sys.time() - start)

    sprintf("Test took: %f", Sys.time() - start)
    return(NULL)
}


parse_files('M:/Projects/WikiGenerator/data/articles', 'M:/Projects/WikiGenerator/data/parsed')
#test_parse()
