# 1 LSTM cell model


library(keras)


source('src/utils.R')


lstm_2 <- function(input_shape, vocab_size) {
    model <- keras_model_sequential() %>%
        layer_lstm(
            units = 64,
            input_shape = input_shape,
            return_sequences = TRUE
        ) %>%
        layer_lstm(
            units = 64,
            return_sequences = TRUE
        ) %>%
        time_distributed(
            layer_dense(
                units = vocab_size,
                activation = 'softmax'
            )
        )
}
