
library(tensorflow)


lstm_net <- function(batch_size, n_hidden, n_vocab, seq_len, pred_len = 1) {

    dt <- tf$float32

    x <- tf$placeholder(dt, shape(NULL, seq_len, n_vocab), name = 'input_layer')
    y <- tf$placeholder(tf$float32, shape(NULL, pred_len, n_vocab), name = 'output_layer')

    x = tf$reshape(x, shape = shape(-1, seq_len))
    x = tf$split(x, seq_len, 1)
    
    weights <- tf$Variable(tf$random_normal(shape(n_hidden, n_vocab), dtype = dt), dtype = dt)
    biases <- tf$Variable(tf$random_normal(shape(n_vocab), dtype = dt), dtype = dt)

    lstm_cell <- tf$contrib$rnn$LSTMCell(n_hidden)

    lstm_layer <- tf$contrib$rnn$static_rnn(lstm_cell, x, dtype=dt)#tf$nn$dynamic_rnn(cell = lstm_cell, inputs = x, initial_state = initial_state, dtype = dt)
    outputs <- lstm_layer[[1]]

    pred <- tf$matmul(outputs[-1], weights) + biases

    return(list(x, y))
}
