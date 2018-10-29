# 1 LSTM cell model


library(keras)


lstm_1 <- function(n_vocab, seq_len) {
    model <- keras_model_sequential()
    model %>%
        layer_embedding(
            #input_dim = n_vocab,
            output_dim = 50,
            input_length = seq_len,
            input_shape = c(seq_len)
        ) %>%
        layer_lstm(
            units = 128
        ) %>%
        time_distributed(
            layer_dense(
                units = n_vocab,
                activation = 'softmax'
            )
        )
}

summary(lstm_1(40, 128))
