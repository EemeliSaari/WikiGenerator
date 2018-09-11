
require(tensorflow)


lstm_net <- function(batch_size, n_hidden, n_vocab) {
    
    tf$reset_default_graph()

    dt <- tf$float32
    
    input_layer <- tf$placeholder(dt, shape(batch_size, NULL, 1), name = 'input_tensor')

    weights <- tf$Variable(tf$random_normal(shape(n_hidden, n_vocab), dtype = dt), dtype = dt)
    biases <- tf$Variable(tf$random_normal(shape(n_vocab), dtype = dt), dtype = dt)

    lstm_cell <- tf$contrib$rnn$LSTMCell(n_hidden)

    initial_state <- lstm_cell$zero_state(batch_size = batch_size, dtype = dt)

    lstm_layer <- tf$nn$dynamic_rnn(cell = lstm_cell, inputs = input_layer, initial_state = initial_state, dtype = dt)

    outputs <- lstm_layer[[1]]

    pred <- tf$matmul(outputs[-1], weights) + biases
    print(pred)
    return(pred)
}
