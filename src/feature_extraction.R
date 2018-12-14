
source('src/model_generator.R')


encode <- function(raw, ref) {
    encoded <- raw

    ref_values <- unname(ref)
    ref_names <- names(ref)
    for (i in c(1:length(ref))) {
        value <- ref_values[i]
        logic <- raw == value
        len <- length(encoded[logic])
        if (len > 0)
            encoded[logic] <- rep(ref_names[i], len)
    }
    return(encoded)
}


decode <- function(enc, ref) {
    decoded <- enc

    ref_values <- unname(ref)
    ref_names <- names(ref)
    for(i in c(1:length(ref))) {
        value <- ref_values[i]
        logic <- enc == ref_names[i]
        len <- length(decoded[logic])
        if (len > 0)
            decoded[logic] <- rep(unname(value), len)
    }
    return(decoded)
}


update_chars <- function(old, new) {
    new_characters <- new[!(new %in% old)]
    if (length(new_characters) > 0) {
        index <- min(which(is.na(old)))
        if (index + length(new_characters) > length(old))
            old[index:length(old)] = new_characters[1:(length(old) - index)]
        else
            old[index:(index + length(new_characters) - 1)] = new_characters
    }
    return(old)
}


from_categorical <- function(X) {
    unlist(lapply(X, FUN=function(x) which(x == 1)))
}


text_vectorizer <- function(data, chars = NULL, update = T, categorical = T) {
    letters <- strsplit(data, '')[[1]]

    uniq <- unique(unlist(letters))
    if (!is.null(chars) && update)
        chars <- update_chars(chars, uniq)
    else
        chars <- uniq

    if (is.null(names(chars)))
        names(chars) <- c(1:length(chars))

    encoded <- unlist(lapply(letters, FUN=function(x) strtoi(encode(x, chars))))

    len <- length(chars)

    dataset <- encoded
    if (categorical)
        dataset = lapply(encoded, FUN=function(x){
                zeros <- rep(0, len)
                zeros[x] = 1
                out <- zeros
            }
        )

    return(list(dataset, chars))
}


sequencing <- function(vec, seq_length=5, shift=1) {
    X <- list()
    y <- list()
    for (i in seq((1 + shift), (length(vec) - (shift + seq_length)), shift)) {
        X[[i - 1]] <- vec[i:(i + seq_length - 1)]
        y[[i - 1]] <- vec[(i + shift + seq_length - 1)]
    }
    return(list(X, y))
}
