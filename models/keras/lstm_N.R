# N LSTM cell model

library(keras)


lstm_N <- function(n_vocab, seq_len, n_hidden = 256, n_layers = 1) {
    model <- keras_model_sequential()

    model <- model %>% bidirectional(
        input_shape = c(seq_len, n_vocab),
        layer_lstm(
            units = n_hidden,
            return_sequences = T
        )
    )
    model %>% layer_dropout(0.2)
    for(i in c(1:(n_layers - 1))) {
        model %>% bidirectional(
            layer_lstm(
                units = n_hidden,
                return_sequences = i < (n_layers - 1)
            )
        )
        model %>% layer_dropout(0.2)
    }
    model %>% layer_dense(
        units = n_vocab,
        activation = 'softmax'
    )
    return(model)
}
