
require(tensorflow)


lstm_net <- function(options) {

    tf$reset_default_graph()
    
    input_layer <- tf$placeholder(tf$float32, shape(options$batch_size, NULL, 1), name = 'input_tensor')

    weights <- tf$Variable(tf$random_normal(shape(options$n_hidden, options$n_vocab), dtype = tf$float32), dtype=tf$float32)
    biases <- tf$Variable(tf$random_normal(shape(options$n_vocab), dtype = tf$float32), dtype = tf$float32)

    lstm_cell <- tf$contrib$rnn$LSTMCell(options$n_hidden)

    initial_state <- lstm_cell$zero_state(options$batch_size, dtype=tf$float32)
    lstm_layer <- tf$nn$dynamic_rnn(cell = lstm_cell, inputs = input_layer, initial_state = initial_state, dtype = tf$float32)

    outputs <- lstm_layer[[1]]

    pred <- tf$matmul(outputs[-1], weights) + biases

    return(pred)
}
