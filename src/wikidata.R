library(methods)


WikiData <- setClass(
    Class = 'WikiData',

    slots = c(
        raw = 'character',
        parts = 'list'
    )
)
