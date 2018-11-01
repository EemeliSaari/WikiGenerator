
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

opt <- list(
    n_vocab = 250,
    seq_len = 30,
    n_hidden = 128,
    batch_size = 128,
    pred_len = 1,
    interface = 'tensorflow',
    optimizer = 'AdamOptimizer'
)

model <- lstm_1(opt$n_vocab, opt$seq_len, opt$pred_len)

model %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_adam(),
    metrics = c('accuracy')
)

summary(model)

epochs_per_file <- 10
weights_path <- 'M:/Projects/WikiGenerator/data/weights/'

generator_from_files <- function(directory) {
    
    files <- list.files(directory)
    n_files <- length(files)
    next_file <- 0

    chars <- seq()

    function(n = 0) {
        next_file <<- next_file + 1

        if (next_file > length(files))
            next_file <<- 1

        file <- files[next_file + n]

        path <- file.path(directory, file)
        parsed <- lapply(unlist(readLines(path)), FUN=function(x) {if(x != '') x else NULL})
        parsed <- parsed[-which(sapply(parsed, is.null))]

        text <- paste(unlist(parsed), collapse = '<br />')

        c(dataset, chars) := text_vectorizer(text)

        c(X, y) := sequencing(dataset, seq_length = opt$seq_len)

        X_train <- array(0, dim = c(length(X), opt$seq_len, opt$n_vocab))
        for(i in 1:length(X))
            X_train[i, ,] = matrix(unlist(X[[i]]), nrow = opt$seq_len, ncol = opt$n_vocab)
        y_train <- matrix(unlist(y), nrow = length(y), ncol = opt$n_vocab)

        X <- c()
        y <- c()
        gc()
        return(list(X_train, y_train))
    }
}

store_metrics <- function(path, history, delimiter = ';') {
    metrics <- history$metrics
    
    lines <- c()
    if (!file.exists(path)) {
        headers <- paste(names(metrics), collapse = ';')
        lines = c(headers)
    }
    for(i in c(1:length(metrics[[1]]))) {
        lines = c(lines, paste(unlist(lapply(names(metrics), FUN=function(x) metrics[[x]][i])), collapse = ';'))
    }
    lapply(lines, write, file=path, sep='\n', append=T)
    return(NULL) 
} 

gen <- generator_from_files('data/articles/')
training_results <- '/data/training_results.csv'

train_steps <- 10
for(step in c(1:train_steps)) {

    c(X_train, y_train) := gen()
    c(X_test, y_test) := gen(1)

    history <- model %>% fit(
        X_train, y_train, epochs=5, 
        batch_size = opt$batch_size, verbose = 1,
        validation_data = list(X_test, y_test)
        #callbacks = callback_model_checkpoint(weights_path)
    )
    save_model_hdf5(model, file.path(weights_path, paste('weights_', toString(step), '.hdf')))
    store_metrics(training_results, history)
}