
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


text_vectorizer <- function(data) {
    letters <- strsplit(data, '')[[1]]

    chars <- unique(unlist(letters))
    names(chars) <- c(1:length(chars))

    encoded <- unlist(lapply(letters, FUN=function(x) strtoi(encode(x, chars))))

    len <- length(chars)

    dataset <- encoded
    dataset <- lapply(encoded, FUN=function(x){
            zeros <- rep(0, len)
            zeros[x] = 1
            out <- zeros
        }
    )
    
    return(list(dataset, chars))
}


sequencing <- function(vec, seq_length=5, step=1) {
    X <- list()
    y <- list()
    for (i in seq(2, (length(vec)-(1 + seq_length)), step)) {
        X[[i - 1]] <- vec[i:(i + seq_length - 1)]
        y[[i - 1]] <- vec[(i + 1):(i + 1)]
    }
    return(list(X, y))
}

parsed <- lapply(unlist(readLines('data/articles/13422.txt')), FUN=function(x) {if(x != '') x else NULL})
parsed <- parsed[-which(sapply(parsed, is.null))]

text <- paste(unlist(parsed), collapse = '<br />')

c(dataset, chars) := text_vectorizer(text)

#print(chars)
#print(dataset)

batch_generator <- function(options) {
    #c(options, )
}

opt <- list(
    batch_size = 128,
    n_vocab = length(chars),
    seq_len = 40,
    n_hidden = 128,
    pred_len = 1,
    interface = 'tensorflow',
    optimizer = 'AdamOptimizer'
)
tf$reset_default_graph()

c(net, output_layer) := generate_model(opt)

c(X, y) := sequencing(dataset, seq_length = opt$seq_len)

sess <- tf$Session()
sess$run(tf$global_variables_initializer())

input_layer <- sess$graph$get_tensor_by_name('input_layer:0')
output_layer <- sess$graph$get_tensor_by_name('output_layer:0')

for(i in seq(1, length(X), opt$batch_size)) {
    batch_X <- X[i:opt$batch_size]
    batch_y <- y[i:opt$batch_size]
    sess$run(net, feed_dict = dict(input_layer = batch_X, output_layer = batch_y))
}
sess$close()
