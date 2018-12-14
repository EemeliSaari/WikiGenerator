library(tools)


generate_dataset <- function(options) {
    
    c(options, keyword) := pop_from_list(options, "keyword")
    c(options, articles) := pop_from_list(options, "articles")
    c(options, ids) := pop_from_list(options, "ids")
    
    
}

raw_lines <- lapply(readLines(opt$ids), FUN=function(line) parse_line(line, ';'))

headers <- raw_lines[[1]]
df <- as.data.frame(do.call(rbind, raw_lines[2:length(raw_lines)]))
names(df) = headers

key <- tolower(opt$key)
pattern <- paste(key, '|', toTitleCase(key), sep = '', collapse = '')

titles <- as.vector(unlist(df$title))
ids <- as.vector(unlist(df$ns))

matches <- grep(pattern, titles)
matched_titles <- unlist(lapply(matches, FUN=function(i) titles[i]))
matched_ids <- unlist(lapply(matches, FUN=function(i) ids[i]))

print('Found the following matches:')
print(matched_titles)

gc()


dataset <- lapply(c(1:length(matched_ids)), FUN=function(i) {
        id <- matched_ids[i]
        filename <- paste(id, '.txt', sep = '', collapse = '')
        path <- file.path(opt$articles, filename)
        if (!file.exists(path)) {
            error_text <- paste('Could not find article:', matched_titles[i])
            print(error_text)
            NULL
        }
        else {
            readLines(file.path(opt$articles, filename))
        }
    }
)

dataset = unlist(dataset[-which(sapply(dataset, is.null))])

train_split <- 0.8

split_index <- floor(length(dataset) * train_split)
train <- dataset[1:split_index]
test <- dataset[split_index:length(dataset)]

train_path <- paste(key, '_train.txt', sep = '', collapse = '')
test_path <- paste(key, '_test.txt', sep = '', collapse = '')

stored <- lapply(train, write, file=train_path, sep='\n', append=T)
stored <- lapply(test, write, file=test_path, sep='\n', append=T)