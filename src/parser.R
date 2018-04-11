# Scripts to parse Wikitext

library(methods)


WikiTextParser <- setRefClass(
    Class = 'WikiTextParser',
    fields = list(
        raw_text = 'character',
        text_parts = 'list',
        options = 'vector'
    ),
    methods = list(
        parse_all_text = function() {
            #TODO Parse all the parts to a string format
        },
        parse_parts = function() {
            #TODO parse different parts of the text
        },
        load_file = function(file_path) {
            "Load the file to the raw_text"
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
