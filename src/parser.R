# Scripts to parse Wikitext

source('src/wikiparser.R')


parse_files <- function(folder, target) {
    "Parse all the wikitext files from a folder."
    #TODO
}


test_parse <- function() {
    example_data <- unlist(readLines('data/articles/113311.txt'))
    test_options <- c(
        'files', 'sitations', 'tags',
        'links_internal', 'links_external', 'category',
        'sections', 'formating', 'list',
        'identing', 'hidden'
    )
    print(typeof(test_options))

    parser <- WikiParser(options = test_options)
    print('asd')
    print(typeof(parser))
    print(typeof(example_data))
    start <- Sys.time()
    print(parse_text(parser, example_data))
    sprintf("Test took: %f", Sys.time() - start)
    return(NULL)
}

test_parse()
