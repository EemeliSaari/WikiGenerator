# Scripts to parse Wikitext

library(methods)


WikiTextParser <- setRefClass(
    Class = 'WikiTextParser',
    fields = list(
        engine = 'S4',
        data = 'S4'
    ),
    methods = list(
        init = function(raw_data) {
            
        }
    )
)


WikiNode <- setRefClass(
    Class = 'WikiNode',
    fields = list(
        type = 'character'
    ),
    methods = list(
        run = function() {
            #TODO
        }
    )
)


ParserEngine <- setRefClass(
    Class = 'WikiTextParser',
    fields = list(
        raw_text = 'character',
        text_parts = 'list',
        options = 'list'
    ),
    methods = list(
        get_parsed_text = function() {
            
        },
        parse_sections = function() {
            #TODO parse different sections of the text
        },
        parse_list = function() {
            #TODO parse different lists
        },
        parse_identity_text = function() {
            #TODO
        },
        parse_redirects = function() {
            #TODO
        },
        parse_sitations = function() {
            #TODO
        },
        parse_links = function() {
            #TODO
        },
        parse_tags = function() {
            tags <- c('u', 's', 'p')
            #TODO
        },
        parse_text_formating = function() {
              
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
        get_valid_options = function() {
            #TODO returns a summary of options
        }
    )
)


parse_files <- function(folder, target) {
    "Parse all the wikitext files from a folder."
    #TODO
}


test_parse <- function() {
    example_data <- unlist(readLines('data/articles/12055.txt'))
    test_options <- list(tags = FALSE)
    print(typeof(test_options))

    parser <- ParserEngine(raw_text = example_data, options = test_options)
    print(typeof(parser))
    parser$get_parsed_text()
    
    return(NULL)
}

test_parse()
