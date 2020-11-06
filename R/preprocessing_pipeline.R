preprocessing_pipeline <- function(choices  ,
                                   text,
                                   infrequent_term_threshold = .01,
                                   verbose = FALSE,
                                   save_dfm = FALSE,
                                   intermediate_directory = NULL)
{
    stopwords <- if(choices$removeStopwords) quanteda::stopwords() else FALSE
    if(choices$use_ngrams){ #need to tokenize before ngram before preprocessing
        text <- quanteda::tokens(text)
        text <- quanteda::tokens_ngrams(text, n = 1:3)
    }
    
    text <- quanteda::dfm(text,
                       tolower = choices$lowercase,
                       stem = choices$stem,
                       remove = stopwords,
                       remove_punct = choices$removePunctuation,
                       remove_numbers = choices$removeNumbers,
                       verbose = verbose)

    if (choices$infrequent_terms) {
        text <- remove_infrequent_terms(
            dfm_object = text,
            proportion_threshold = infrequent_term_threshold,
            verbose = verbose)
    }

    if (save_dfm & !is.null(intermediate_directory)){
        save(current_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))
    }
    
    return(text)
}
