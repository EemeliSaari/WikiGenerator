
library(methods)


MODELS_BASE <- 'models'


get_available_models <- function(name) {
    paths <- file.path(MODELS_BASE, name)
    models <- list.files(paths)
    for (f in models) {
        source(file.path(paths, f))
    }
    return(models)
}


get_optimizer <- function(name, opts=list()) {
    command <- paste('tf$train$', name, sep='')
    optimizer <- do.call(eval(parse(text = command)), args=c(opts))
    
    return(optimizer)
}


generate_model <- function(options) {
    "Model training process"

    c(options, interface) := pop_from_list(options, "interface")

    get_available_models(interface)

    c(options, optimizer) := pop_from_list(options, "optimizer")

    c(options, batch_size) := pop_from_list(options, "batch_size")
    c(options, n_hidden) := pop_from_list(options, "n_hidden")
    c(options, n_vocab) := pop_from_list(options, "n_vocab")
    c(options, seq_len) := pop_from_list(options, "seq_len")
    c(options, pred_len) := pop_from_list(options, "pred_len", 1)

    c(net, y) := lstm_net(batch_size, n_hidden, n_vocab, seq_len, pred_len)

    cost <- tf$reduce_mean(tf$nn$softmax_cross_entropy_with_logits(logits=net, labels=y))

    optimizer <- get_optimizer(optimizer, options)

    net <- optimizer$minimize(cost)

    return(list(net, y))
}
