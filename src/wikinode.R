library(methods)


WikiNode <- setRefClass(
    Class = 'WikiNode',
    fields = list(
        type = 'character',
        style = 'list',
        content = 'character',
        tokens = 'numeric'
    )
)
