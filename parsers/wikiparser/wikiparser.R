library(methods)


#Map different language specific words
LANG_MAP <- list(
    file = list(
        eng = 'File',
        fi = 'Tiedosto'
    ),
    category = list(
        eng = 'Category',
        fi = 'Luokka'
    )
)


WikiParser <- setClass(
    Class = 'WikiParser',

    slots = c(
        options = 'vector',
        engine = 'ParserEngine',
        data = 'WikiData',
        verbose = 'logical'
    ),

    prototype = list(
        options = c(
            'file', 'sitations', 'tags', 'special_character',
            'links_internal', 'links_external', 'category',
            'sections', 'formating', 'list',
            'identing', 'hidden', 'table'
        ),
        engine = ParserEngine(lang_map = LANG_MAP, lang = 'fi'),
        data = WikiData(),
        verbose = TRUE
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
        if (obj@verbose) {
            print('Starting the parsing...')
        }
        return(lapply(obj@engine$get_parts(text), FUN = function(x) {
            entry <- unlist(x)
            for (i in c(1:length(entry))) {
                for (option in obj@options) {
                    command <- paste('obj@engine$parse_', option, '(entry[i])', sep = '')
                    entry[i] <- eval(parse(text = command))
                }
            }
            out <- entry
        }))
        if (obj@verbose) {
            print('...Parsing done')
        }
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
    }
)
