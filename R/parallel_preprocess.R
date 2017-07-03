parallel_preprocess <- function(i,
                                choices,
                                text,
                                infrequent_term_threshold,
                                intermediate_directory){

    setwd(intermediate_directory)

    verbose = FALSE

    cat("Currently working on combination",i,"of",nrow(choices),"\n")
    ptm <- proc.time()

    # need a conditional for removing stopwords
    if (choices$removeStopwords[i]) {
        # generate dfm
        if (choices$use_ngrams[i]) {
            if (choices$removePunctuation[i]) {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        ngrams = 1:3,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = TRUE,
                        remove_numbers = FALSE,
                        ngrams = 1:3,
                        verbose = verbose)
                }
            } else {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = FALSE,
                        remove_numbers = TRUE,
                        ngrams = 1:3,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = FALSE,
                        remove_numbers = FALSE,
                        ngrams = 1:3,
                        verbose = verbose)
                }
            }
        } else {
            if (choices$removePunctuation[i]) {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = TRUE,
                        remove_numbers = FALSE,
                        verbose = verbose)
                }
            } else {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = FALSE,
                        remove_numbers = TRUE,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove = quanteda::stopwords(),
                        remove_punct = FALSE,
                        remove_numbers = FALSE,
                        verbose = verbose)
                }
            }
        }
    } else {
        # generate dfm
        if (choices$use_ngrams[i]) {
            if (choices$removePunctuation[i]) {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        ngrams = 1:3,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = TRUE,
                        remove_numbers = FALSE,
                        ngrams = 1:3,
                        verbose = verbose)
                }
            } else {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = FALSE,
                        remove_numbers = TRUE,
                        ngrams = 1:3,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = FALSE,
                        remove_numbers = FALSE,
                        ngrams = 1:3,
                        verbose = verbose)
                }
            }
        } else {
            if (choices$removePunctuation[i]) {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = TRUE,
                        remove_numbers = TRUE,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = TRUE,
                        remove_numbers = FALSE,
                        verbose = verbose)
                }
            } else {
                if (choices$removeNumbers[i]) {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = FALSE,
                        remove_numbers = TRUE,
                        verbose = verbose)
                } else {
                    current_dfm <- quanteda::dfm(
                        x = text,
                        tolower = choices$lowercase[i],
                        stem = choices$stem[i],
                        remove_punct = FALSE,
                        remove_numbers = FALSE,
                        verbose = verbose)
                }
            }
        }
    }

    # remove infrequent terms
    if (choices$infrequent_terms[i]) {
        current_dfm <- remove_infrequent_terms(
            dfm_object = current_dfm,
            proportion_threshold = infrequent_term_threshold,
            verbose = verbose)
    }

    t2 <- proc.time() - ptm
    ret <- paste("Complete in:",t2[[3]],"seconds")

    # store the current dfm
    save(current_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))

    return(ret)
}
