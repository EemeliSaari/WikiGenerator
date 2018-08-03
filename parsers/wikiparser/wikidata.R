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
        standardGeneric('to_text')
    }
)

setMethod(
    f = 'to_text',
    signature = 'WikiData',
    definition = function(obj) {
        #TODO
    }
)
