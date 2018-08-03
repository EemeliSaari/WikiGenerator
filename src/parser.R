# Scripts to parse Wikitext


for(f in list.files('parsers/wikiparser')) {
    source(paste('parsers/wikiparser/', f, sep = ''))
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


parse_files <- function(folder, target, parser_options = c(), verbose = 0, options = NULL) {
    "Parse all the wikitext files from a folder."
    
    # If options are given - override all the other parameters
    if (!is.null(options)) {
        folder <- options$folder    
        target <- options$target_folder
        parser_options <- options$args[2:length(opts$args)]
        verbose <- options$verbose
    }

    if (!file.exists(folder)) {
        print(paste('ERROR path: ', folder, ' does not exists.', sep = ''))
        return(NULL)
    }
    if (!file.exists(target) && !dir.create(target)) {
        print(paste('ERROR target path ', target, ' is invalid.', sep = ''))
        return(NULL)
    }

    verbose_bool <- FALSE
    if (!verbose %in% c(0, 1)) {
        print(paste('Invalid verbose value: ', verbose, sep = ''))
        return(NULL)
    }
    else if(verbose == 1) {
        verbose_bool = TRUE
    }

    if (length(parser_options) == 0) {
        parser <- WikiParser(verbose = verbose_bool)
    }
    else {
        parser <- WikiParser(options = parser_options, verbose = verbose_bool)
    }

    results <- lapply(list.files(folder), FUN = function(x) {
            file_path <- file.path(folder, x)
            target_path <- file.path(target, x)
            parsed <- parse_text(parser, unlist(readLines(file_path)))
            parts_to_files(parsed, target_path)
        }
    )
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
