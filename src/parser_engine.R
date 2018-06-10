library(methods)


ParserEngine <- setRefClass(
    Class = 'ParserEngine',
    fields = list(
        lang = 'character'
    ),
    methods = list(
        parse_sections = function(x) {
            patterns <- list(
                list(pattern = '=+([^=]*)=+', group = '\\1', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_list = function(x) {
            patterns <- list(
                list(pattern = '^((\\*)|(\\#))+\\s', group = '', perl = FALSE),
                list(patterns= '^((\\:)|(\\;))\\s', group = '', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_identing = function(x) {
            patterns <- list(
                list(pattern = '^\\:+(?!\\s)', group = '', perl = TRUE)
            )
            return(parse_job(patterns, x))
        },
        parse_sitations = function(x) {
            patterns <- list(
                list(pattern = '\\{{2}([^\\}]*)\\}{2}', group = '', perl = FALSE)
            )
            parsed <- parse_job(patterns, x)
            return(gsub('}', '', parsed))
        },
        parse_links_internal = function(x) {
            gr <- gregexpr('\\[[^]]*\\]{2}', x)
            matches <- regmatches(x, gr)
            # Capture all the groups and apply logic
            regmatches(x, gr)[[1]] = lapply(matches[[1]], FUN = function(mat) {
                    if (grepl('Luokka:', mat)) {
                        return(mat)
                    }
                    parts <- strsplit(mat, '')
                    print(mat)
                    if (length(parts) == 0) {
                        if ('(' %in% parts) {
                            out <- ''
                        }
                        else {
                            out <- mat
                        }
                    }
                    else {
                        parts <- parts[[1]]
                        pipe_index <- match('|', parts)
                        # Handle pipe condition if it was found
                        if (!is.na(pipe_index)) {
                            if (pipe_index < length(parts) - 2) {
                                # Capture everything after pipe
                                out <- paste(parts[(pipe_index + 1):(length(parts) - 2)], collapse = '')
                            }
                            else {
                                colon_index <- match(':', parts)
                                if (!is.na(colon_index)) {
                                    out <- paste(parts[(colon_index + 1):pipe_index], collapse = '')
                                }
                                else {
                                    out <- paste(parts[3:pipe_index], collapse = '')
                                }
                            }
                        }
                        else {
                            out <- paste(parts[3:(length(parts) - 2)], collapse = '')
                        }
                    }
                }
            )
            return(x)
        },
        parse_links_external = function(x) {
            patterns <- list(
                list(pattern = '(?<!\\[)\\[[^\\]\\s]*([^\\]]*)?\\](?!\\])', group = '\\1', perl = TRUE),
                list(pattern = 'https?://[^\\s]*', group = '', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_tags = function(x) {
            patterns <- list(
                list(pattern = '<math>.*<\\/math>', group = ''),
                list(pattern = '<.*?>', group = '')
            )
            return(parse_job(patterns, x))
        },
        parse_formating = function(x) {
            patterns <- list(
                list(pattern = "'{2,}([^']*)'{2,}", group = '\\1', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_hidden = function(x) {
            patterns <- list(
                list(pattern = '<!--.*-->', group = '', perl = FALSE),
                list(pattern = '<!--.*', group = '', perl = FALSE),
                list(pattern = '.*-->', group = '', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_files = function(x) {
            name_map <- list(
                eng = 'File',
                fi = 'Tiedosto'
            )
            if (is.null(name_map[[lang]])) {
                warning(paste('Invalid language flag: ', lang, sep = ''))
                return(x)
            }
            patterns <- list(
                list(pattern = paste('\\[{2}((', name_map[[lang]], ')|(File)):[^]]*\\]{2}', sep = ''), group = '', perl = FALSE),
                list(pattern = paste('^((', name_map[[lang]], ')|(File)):([^\\|]*\\|)+', sep = ''), group = '', perl = FALSE)
            )
            return(parse_job(patterns, x))
        },
        parse_category = function(x) {
            name_map <- list(
                        eng = 'Category',
                        fi = 'Luokka'
            )
            if (is.null(name_map[[lang]])) {
                warning(paste('Invalid language flag: ', lang, sep = ''))
                return(x)
            }
            pattern <- paste('\\[\\[', name_map[[lang]], ':[^]]*\\]\\]', sep = '')
            gr <- gregexpr(pattern, x)
            matches <- regmatches(x, gr)
            # Capture all the groups and apply logic
            regmatches(x, gr)[[1]] = lapply(matches[[1]], FUN = function(mat) {
                    parts <- strsplit(mat, '')[[1]]
                    pipe <- !grepl('|', mat)
                    # Link to a category
                    if (':' == parts[3] && !pipe) {
                        out <- paste(parts[4:(length(parts) - 2)])
                    }
                    # Without prefix
                    else if (':' == parts[3] && pipe) {
                        indices <- which(parts %in% ':')
                        # Pick the last match of the ':'
                        out <- paste(parts[(indices[length(indices)] + 1):(length(parts) - 3)], sep = '', collapse = '')
                    }
                    else {
                        out <- ''
                    }
                }
            )
            return(x)
        },
        get_parts = function(raw) {
            index <- 1
            parts <- list()
            for (line in raw) {
                if (line == '') {
                    index = index + 1
                    next
                }
                if (length(parts) < index) {
                    parts[[index]] <- c(line)
                }
                else {
                    parts[[index]] <- c(parts[[index]], line)
                }
            }
            return(parts)
        },
        parse_job = function(patterns, text) {
            parse_env <- new.env()
            parse_env$text = text
            lapply(patterns, FUN = function(x) {
                    parse_env$text = gsub(x$pattern, x$group, parse_env$text, perl = x$perl)
                }
            )
            return(parse_env$text)
        }
    )
)
