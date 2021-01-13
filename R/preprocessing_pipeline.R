## pipeline to preprocess the corpus
## TODO modular set your own pipeline
stopwordprocess <- function(removeStopwords, custom_stopwords = NULL, language){
    if(removeStopwords){ #bool 
        if(!is.null(custom_stopwords)){
            if(is.character(custom_stopwords)){
                removeStopwords <- custom_stopwords
            }else{
                stop("ERROR: custom_stopwords must be formatted as a character vector of strings. Such as stopwords::stopwords()")
            }
        }else{
            removeStopwords <-  quanteda::stopwords(language = language)}}
    return(removeStopwords)
}

preprocessing_pipeline <- function(choices  ,
                                   text,
                                   infrequent_term_threshold = .01,
                                   verbose = FALSE,
                                   save_dfm = FALSE,
                                   intermediate_directory = NULL,
                                   custom_stopwords = NULL){
    padding <- ifelse(choices$use_ngrams, TRUE, FALSE) # keep whitespace during tokens removal otherwise nonsensical ngram are created.
    text <- quanteda::tokens(text,
                             remove_punct = choices$removePunctuation,
                             remove_numbers = choices$removeNumbers,
                             padding = padding)
    if(choices$lowercase){
        text <- quanteda::tokens_tolower(text, keep_acronyms = FALSE)
    }
    if(choices$removeStopwords){
        stopwords <- stopwordprocess(choices$removeStopwords, custom_stopwords, meta(text)$language)
        text <- quanteda::tokens_remove(text, stopwords, padding = padding)
    }
    if(choices$stem){
        text <- quanteda::tokens_wordstem(text, language = meta(text)$language)
}
    if(choices$use_ngrams){ #need to tokenize before ngram before preprocessing
        text <- quanteda::tokens_ngrams(text, n = 1:3)
    }

    text_dfm <- quanteda::dfm(text, verbose = verbose)
    
    if (choices$infrequent_terms) {
        text <- remove_infrequent_terms(
            dfm_object = text_dfm,
            proportion_threshold = infrequent_term_threshold,
            verbose = verbose)
    }

    if (save_dfm & !is.null(intermediate_directory)){
        save(text_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))
    }
    
    return(text_dfm)
}


