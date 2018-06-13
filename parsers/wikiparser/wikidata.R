library(methods)


WikiData <- setClass(
    Class = 'WikiData',

    slots = c(
        raw = 'character',
        parts = 'list',
        nodes = 'list'
    )
)

setGeneric(
    name = 'to_text',
    def = function(obj) {
        standardGeneric('clear')
    }
)

setMethod(
    f = 'to_text',
    signature = 'WikiParser',
    definition = function(obj) {
        
    }
)
