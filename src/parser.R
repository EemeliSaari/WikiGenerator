# Scripts to parse Wikitext

library(methods)


WikiData <- setClass(
    Class = 'WikiData',

    slots = c(
        raw = 'character',
        parts = 'list'
    )
)


WikiNode <- setRefClass(
    Class = 'WikiNode',
    fields = list(
        type = 'character',
        style = 'list',
        content = 'character',
        tokens = 'numeric'
    )
)


ParserEngine <- setRefClass(
  Class = 'ParserEngine',

  methods = list(
        parse_sections = function() {
            #TODO parse different sections of the text
        },
        parse_list = function() {
            #TODO parse different lists
        },
        parse_identity_text = function() {
            #TODO
        },
        parse_sitations = function() {
            #TODO
        },
        parse_links_internal = function(x) {
            gr <- gregexpr('\\[[^]]*\\]{2}', x)
            matches <- regmatches(x, gr)
            regmatches(x, gr) = lapply(matches, FUN = function(mat) {
                    parts <- strsplit(mat, '')[[1]]
                    pipe_index <- match('|', parts)
                    if (!is.na(pipe_index)) {
                        # Handle pipe condition

                        if (pipe_index < length(parts) - 2) {
                            # Capture everything after pipe
                            out <- paste(parts[pipe_index:(length(parts) - 2)])
                        }
                        else {
                            colon_index <- match(':', parts)
                            if (!is.na(colon_index)) {
                                out <- paste(parts[colon_index:pipe_index], collapse = '')
                            }
                            else {
                                out <- paste(parts[3:pipe_index])
                            }
                        }
                    }
                    else {
                        out <- paste(parts[3:(length(parts) - 2)])
                    }
                }
            )
            return(x)
        },
        parse_links_external = function(x) {
            patterns <- list(
                list(pattern = '\\[.*?\\s(.*?)\\]', group = '\\1'),
                list(pattern = 'https?://[^\\s]*', group = '')
            )
            return(parse_method(patterns, x))
        },
        parse_tags = function(x) {
            patterns <- list(
                list(pattern = '<.*?>', group = '')
            )
            return(parse_method(patterns, x))
        },
        parse_text_formating = function() {
            #TODO
        },
        parse_hidden = function() {
            #TODO
        },
        parse_images = function() {
            #TODO
        },
        parse_category = function() {
            #TODO
        },
        get_parts = function(raw) {
            index <- 1
            parts <- list()
            for (line in raw) {
                if (line == '') {
                    index = index + 1
                    next
                }
                if (length(parts) < index) {
                    parts[[index]] <- c(line)
                }
                else {
                    parts[[index]] <- c(parts[[index]], line)
                }
            }
            return(parts)
        },
        parse_method = function(patterns, text) {
            parse_env <- new.env()
            parse_env$text = text
            lapply(patterns, FUN=function(x) {
                    parse_env$text = gsub(x$pattern, x$group, parse_env$text)
                }
            )
            return(parse_env$text)
        }
    )
)


WikiParser <- setClass(
    Class = 'WikiParser',

    slots = c(
        options = 'list',
        engine = 'ParserEngine',
        data = 'WikiData'
    ),

    prototype = list(
        options = list(
            #TODO
        ),
        engine = ParserEngine(),
        data = WikiData()
    ),
    
    validity = function(object) {
        #TODO check the options
        return(TRUE)
    }
)


setGeneric(
    name = 'parse_text',
    def = function(obj, text) {
        standardGeneric('parse_text')
    }
)


setMethod(
    f = 'parse_text',
    signature = 'WikiParser',
    definition = function(obj, text) {
        print('Starting the parsing...')
        return(lapply(obj@engine$get_parts(text), FUN=function(x) {
            entry <- x
            for (option in names(obj@options)) {
                if (obj@options[[option]] == TRUE) {
                    command <- paste('obj@engine$parse_', option, '(entry)', sep='')
                    entry <- eval(parse(text=command))
                }
            }
            out <- entry
        }))
    }
)


setGeneric(
    name = 'set_options',
    def = function(obj, options) {
        standardGeneric('set_options')
    }
)


setMethod(
     f = 'set_options',
     signature = 'WikiParser',
     definition = function(obj, options) {
        "Configures the parser options - takes in a named list of options:"

     }
)


setGeneric(
    name = 'clear',
    def = function(obj) {
        standardGeneric('clear')
    }
)


setMethod(
    f = 'clear',
    signature = 'WikiParser',
    definition = function(obj) {
        obj$data <- WikiData()
        #return(NULL)
    }
)

parse_files <- function(folder, target) {
    "Parse all the wikitext files from a folder."
    #TODO
}


test_parse <- function() {
    example_data <- unlist(readLines('data/articles/13422.txt'))
    test_options <- c('', '', '')
    print(typeof(test_options))

    parser <- WikiParser(options = test_options)
    print('asd')
    print(typeof(parser))
    print(typeof(example_data))

    print(parse_text(parser, example_data))
    return(NULL)
}

test_parse()
