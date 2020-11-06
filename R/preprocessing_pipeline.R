:w

preprocessing_pipeline <- function(t  , choices, infrequent_term_threshold, verbose){
    stopwords <- ifelse(choices$removeStopwords,
                        quanteda::stopwords(),
                        FALSE)
    ngrams <- ifelse(choices$use_ngrams,
                     1:3,
                     FALSE)
    x <- quanteda::dfm(corpus(t),
                       tolower = choices$lowercase,
                       stem = choices$stem,
                       remove = stopwords,
                       remove_punct = choices$removePunctuation,
                       remove_numbers = choices$removeNumbers,
                       ngrams = ngrams,
                       verbose = verbose)

    if (choices$infrequent_terms) {
        x <- remove_infrequent_terms(
            dfm_object = x,
            proportion_threshold = infrequent_term_threshold,
            verbose = verbose)
    }
    
    return(x)
}
