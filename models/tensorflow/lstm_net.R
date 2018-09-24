
require(tensorflow)


lstm_net <- function(batch_size, n_hidden, n_vocab) {
    
    tf$reset_default_graph()

    dt <- tf$float32
    
    x <- tf$placeholder(dt, shape(batch_size, NULL, 28), name = 'input_layer')
    y <- tf$placeholder(tf$float32, shape(batch_size, 10), name = 'output_layer')

    weights <- tf$Variable(tf$random_normal(shape(n_hidden, n_vocab), dtype = dt), dtype = dt)
    biases <- tf$Variable(tf$random_normal(shape(n_vocab), dtype = dt), dtype = dt)

    lstm_cell <- tf$contrib$rnn$LSTMCell(n_hidden)

    initial_state <- lstm_cell$zero_state(batch_size = batch_size, dtype = dt)

    lstm_layer <- tf$nn$dynamic_rnn(cell = lstm_cell, inputs = x, initial_state = initial_state, dtype = dt)

    outputs <- lstm_layer[[1]]

    pred <- tf$matmul(outputs[-1], weights) + biases

    return(list(pred,y))
}
