
N_VOCAB <- NULL
N_SEQUENCE <- NULL
CHARS <- NULL

generator_from_files <- function(directory, start_index = 0, shuffle = T, 
        max_chars = 250, verbose = T) {

    files <- list.files(directory)
    if(shuffle)
        files <- sample(files)

    n_files <- length(files)
    next_file <- start_index

    chars <- rep(NA, max_chars)
    names(chars) <- c(1:max_chars)
    function(n = 0) {
        next_file <<- next_file + 1 + n
        if (next_file > length(files))
            next_file <<- 1

        file <- files[next_file]
        if(verbose)
            print(paste('Generating from file:', file))
        path <- file.path(directory, file)

        text <- paste(unlist(unlist(readLines(path))), collapse = '\n')

        c(dataset, new_chars) := text_vectorizer(text, chars)

        chars <<- new_chars
        CHARS <<- chars
        c(X, y) := sequencing(dataset, seq_length = opt$seq_len, shift = 1)

        X_train <- array(0, dim = c(length(X), opt$seq_len, opt$n_vocab))
        y_train <- array(0, dim = c(length(y), opt$seq_len, opt$n_vocab))
        for(i in 1:length(X)) {
            X_train[i, , ] = matrix(unlist(X[[i]]), nrow = opt$seq_len, ncol = opt$n_vocab)
            y_train[i, , ] = matrix(unlist(y[[i]]), nrow = opt$seq_len, ncol = opt$n_vocab)
        }
        # Clear the memory to help with big datasets
        gc()
        return(list(X_train, y_train))
    }
}


generator_from_chars <- function(filepath, n_sequence = 100, shift = 1, 
        step_size = 2000, categorical = T, normalize = T,
        verbose = T) {

    text <- paste(unlist(unlist(readLines(filepath))), collapse = '\n')

    c(dataset, chars) := text_vectorizer(text, categorical = categorical)

    CHARS <<- chars
    N_VOCAB <<- length(chars)
    N_SEQUENCE <<- n_sequence

    if (normalize)
        dataset = dataset / N_VOCAB

    c(X, y) := sequencing(dataset, N_SEQUENCE)

    SEQUENCE <- seq(1, length(X), step_size)

    index <- 0
    function() {
        index <<- index + 1
        if (index > length(SEQUENCE)) {
            index <<- 1
            if(verbose)
                print("Finished the sequence - starting over.")
        }
        if(verbose)
            print(index)
        current <- SEQUENCE[index]

        X_train <- array(0, dim = c(step_size, N_SEQUENCE, N_VOCAB))
        y_train <- array(0, dim = c(step_size, N_VOCAB))

        end_index <- current + step_size - shift
        if (end_index > length(X))
            end_index <- length(X)

        indexing <- c(current:end_index)
        for (i in c(1:length(indexing))) {
            X_train[i, , ] = matrix(unlist(X[[indexing[i]]]), nrow = N_SEQUENCE, ncol = N_VOCAB)
            y_train[i, ] = y[[indexing[i]]][[1]]
        }
        gc()
        return(list(X_train, y_train))
    }
}
