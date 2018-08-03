# Main script for the software

options(warn = -1)
require('optparse')
options(warn = 0)

for(f in list.files('src')) {
    source(paste('src/', f, sep = ''))
}

read_args <- function(program) {
    "Read the user commandline arguments"

    program_options = list(
        generate = list(
            make_option(c('-s', '--seed'), type = 'character',
                default = FALSE, help = 'Generator seed'            
            ),
            make_option(c('-m', '--model'), type = 'character',
                default = FALSE, help = 'Trained model used for generaing'            
            )
        ),
        train = list(
            make_option(c('-m', '--model'), type = 'character',
                default = FALSE, help = 'Trainable model name'            
            ),
            make_option(c('-v', '--verbose'), type = 'integer',
                default = 1, help = 'Training verbose mode'       
            ),
            make_option(c('-ds', '--datapath'), type = 'character',
                default = FALSE, help = 'Path for data, must contain metafile'            
            ),
            make_option(c('-o', '--optimizer'), type = 'character',
                default = 'adam', help = 'Model optimizer'            
            ),
            make_option(c('-l', '--loss'), type = 'character',
                default = FALSE, help = 'Loss measured'
            ),
            make_option(c('-u', '--hidden_units'), type = 'integer',
                default = 4, help = 'Number of hidden units'            
            )
        ),
        parse = list(
            make_option(c('-f', '--folder'), type = 'character',
                default = FALSE, help = 'Folder containing text files.'
            ),
            make_option(c('-t', '--target_folder'), type = 'character',
                default = FALSE, help = 'Out folder for parsed files.'            
            ),
            make_option(c('-v', '--verbose'), type = 'integer',
                default = 0, help = 'Verbose on parsing process.'            
            )
        ),
        download = list(
            make_option(c('-lm', '--login_method'), type = 'character',
                default = FALSE, help = 'Login method for WikiMedia'            
            ),
            make_option(c('-f', '--fetch_type'), type = 'character',
                default = FALSE, help = 'Data type to fetch ie. articles'           
            ),
            make_option(c('-p', '--path'), type = 'character',
                default = 'data', help = 'Output path'
            ),
            make_option(c('-l', '--language'), type = 'character',
                default = 'fi', help = 'Language specific setting'
            ),
            make_option(c('-u', '--user_data'), type = 'character',
                default = FALSE, help = 'User data path'
            )
        )
    )
    handle_args(parse_args2(OptionParser(option_list = program_options[[program]])))
}


handle_args <- function(opts) {
    "Handles the option and starts the correct sub-program"
    program <- opts$args[1]
    options <- opts$options
    switch (program,
        'parse' = parse_files(options=options),
        'download' = fetcher(options=options),
        'train' = train_model(options=options),
        'generate' = print('generois')
    )
    return(NULL)
}


check_data <- function() {
    #TODO load data -> load small set if not found -> Parse the data
}


check_model <- function() {
    #TODO load net -> train small if not found
}


# Program entry point
main <- function() {
    argv <- commandArgs(trailingOnly = TRUE)

    arg_options <- c('generate', 'train', 'parse', 'download')
    
    if (length(argv) == 0) {
        print('Provide one of the following arguments:')
        lapply(arg_options, FUN = function(x) {
                print(paste('-', x))
            }
        )
        return(NULL)
    }
    if (!argv[1] %in% arg_options) {
        print(paste('Invalid argument: ', argv[1]))
        return(NULL)
    }

    read_args(argv[1])

    return(1)
}

main()
