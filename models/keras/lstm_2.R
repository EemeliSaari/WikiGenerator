# 2 LSTM cell model


library(keras)


lstm_2 <- function(vocab_size, input_shape=c(NULL, 1)) {
    model <- keras_model_sequential() 
    model %>%
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
