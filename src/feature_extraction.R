
source('src/model_generator.R')

encode <- function(raw, ref) {
    encoded <- raw

    ref_values <- unname(ref)
    ref_names <- names(ref)
    for (i in c(1:length(ref))) {
        value <- ref_values[i]
        logic <- raw == value
        len <- length(encoded[logic])
        if (len > 0) {
            encoded[logic] <- rep(ref_names[i], len)
        }
    }
    encoded
}


decode <- function(enc, ref) {
    decoded <- enc

    ref_values <- unname(ref)
    ref_names <- names(ref)
    for(i in c(1:length(ref))) {
        value <- ref_values[i]
        logic <- enc == ref_names[i]
        len <- length(decoded[logic])
        if (len > 0) {
            decoded[logic] <- rep(unname(value), len)
        }
    }
    decoded
}


letter_vectoriced <- function(data) {
    letters <- lapply(data, FUN=function(x){ strsplit(x, '')[[1]] })

    chars <- unique(unlist(letters))
    names(chars) <- c(1:length(chars))

    encoded <- lapply(letters, FUN=function(x) strtoi(encode(x, chars)))

    return(list(encoded, chars))
}


sequencing <- function(vec, seq_length=5) {

    X <- list()
    y <- list()
    for (i in c(2:(length(vec)-(1 + seq_length)))) {
        X[[i-1]] <- vec[i:(i+seq_length)]
        y[[i-1]] <- vec[(i+1):(i+1+seq_length)]
    }
    return(list(X, y))
}

parsed <- lapply(unlist(readLines('data/articles/12055.txt')), FUN=function(x) {if(x != '') x else NULL})
parsed <- parsed[-which(sapply(parsed, is.null))]

c(dataset, chars) := letter_vectoriced(parsed)

batch_generator <- function(options) {
    #c(options, )
}


opt <- list(
    batch_size = 28,
    n_vocab = 10,
    n_hidden = 128,
    interface = 'tensorflow',
    optimizer = 'AdamOptimizer'
)

c(net, output_layer) := generate_model(opt)

sess <- tf$Session()
sess$run(tf$global_variables_initializer())

c(X, y) := sequencing(dataset[[1]])

input_layer <- sess$graph$get_tensor_by_name('input_layer:0')
#output_layer <- sess$graph$get_tensor_by_name('output_layer:1')
#sess$run(net)
sess$run(net, feed_dict = dict(input_layer = x, output_layer = y))
sess$close()
