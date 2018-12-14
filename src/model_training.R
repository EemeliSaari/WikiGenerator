
source('src/utils.R')


store_metrics <- function(model, history, delimiter = ';') {
    filename <- paste(model$name, '_results.csv', sep='')
    path <- file.path(get_path('results'), filename)
    print(path)
    metrics <- history$metrics
    lines <- c()
    if (!file.exists(path))
        lines = c(paste(names(metrics), collapse = ';'))

    for(i in c(1:length(metrics[[1]])))
        lines = c(lines, paste(unlist(lapply(names(metrics), FUN=function(x) metrics[[x]][i])), collapse = ';'))

    lapply(lines, write, file=path, sep='\n', append=T)
    return(NULL)
}


train_model <- function(model, generator, batch_size = 64, 
        store_freq = 5, train_steps = 25, epochs = 1) {

    for(step in c(1:train_steps)) {
        c(X_train, y_train) := generator()
        history <- model %>% fit(
            X_train, y_train, epochs=epochs, 
            batch_size = batch_size, verbose = 1
        )
        store_metrics(model, history)
        if(step %% store_freq == 0)
            print(store_model_weights(model))
    }
}
