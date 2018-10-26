
library(tensorflow)


lstm_net <- function(batch_size, n_hidden, n_vocab, seq_len, pred_len = 1) {

    #tf$reset_default_graph()

    dt <- tf$float32

    x <- tf$placeholder(dt, shape(NULL, seq_len, n_vocab), name = 'input_layer')
    print(x)
    y <- tf$placeholder(tf$float32, shape(NULL, pred_len, n_vocab), name = 'output_layer')
    print(y)

    x = tf$reshape(x, shape = shape(-1, seq_len))
    x = tf$split(x, seq_len, 1)
    
    print(x)
    weights <- tf$Variable(tf$random_normal(shape(n_hidden, n_vocab), dtype = dt), dtype = dt)
    biases <- tf$Variable(tf$random_normal(shape(n_vocab), dtype = dt), dtype = dt)

    print(weights)
    lstm_cell <- tf$contrib$rnn$LSTMCell(n_hidden)

    lstm_layer <- tf$contrib$rnn$static_rnn(lstm_cell, x, dtype=dt)#tf$nn$dynamic_rnn(cell = lstm_cell, inputs = x, initial_state = initial_state, dtype = dt)
    print(lstm_layer)
    outputs <- lstm_layer[[1]]
    #print(outputs[-1])

    pred <- tf$matmul(outputs[-1], weights) + biases

    return(list(x, y))
}
