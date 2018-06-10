library(methods)


LANG_FLAG = 'fi'


for (script in c('parser_engine', 'wikidata', 'wikinode')) {
    source(paste('src/', script, '.R', sep = '', collapse = ''))
}


WikiParser <- setClass(
    Class = 'WikiParser',

    slots = c(
        options = 'vector',
        engine = 'ParserEngine',
        data = 'WikiData'
    ),

    prototype = list(
        options = c(
            #TODO
        ),
        engine = ParserEngine(lang = LANG_FLAG),
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
        return(lapply(obj@engine$get_parts(text), FUN = function(x) {
            entry <- unlist(x)
            for (i in c(1:length(entry))) {
                for (option in obj@options) {
                    print(option)
                    command <- paste('obj@engine$parse_', option, '(entry[i])', sep = '')
                    print(entry[i])
                    entry[i] <- eval(parse(text = command))
                    print(entry[i])
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
