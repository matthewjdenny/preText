parallel_preprocess <- function(i,
                                choices,
                                text,
                                infrequent_term_threshold,
                                intermediate_directory){

    setwd(intermediate_directory)

    verbose = FALSE

    cat("Currently working on combination",i,"of",nrow(choices),"\n")
    ptm <- proc.time()

    # create initial tokens object:
    current_tokens <- quanteda::tokens(
        x = text,
        remove_punct = choices$removePunctuation[i],
        remove_numbers = choices$removeNumbers[i]
    )

    # add ngrams
    if (choices$use_ngrams[i]) {
        current_tokens <- quanteda::tokens_ngrams(current_tokens,
                                                  n = 1:3)
    }

    # create DFM
    current_dfm <- quanteda::dfm(current_tokens,
                                 tolower = choices$lowercase[i])

    # remove stopwords
    if (choices$removeStopwords[i]) {
        current_dfm <- quanteda::dfm_remove(current_dfm, quanteda::stopwords())
    }

    # remove infrequent terms
    if (choices$infrequent_terms[i]) {
        current_dfm <- remove_infrequent_terms(
            dfm_object = current_dfm,
            proportion_threshold = infrequent_term_threshold,
            verbose = verbose)
    }

    # stem:
    if (choices$stem[i]) {
        current_dfm <- quanteda::dfm_wordstem(current_dfm)
    }

    t2 <- proc.time() - ptm
    ret <- paste("Complete in:",t2[[3]],"seconds")

    # store the current dfm
    save(current_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))

    return(ret)
}
