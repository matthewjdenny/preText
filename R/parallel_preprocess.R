parallel_preprocess <- function(i,
                                choices,
                                text,
                                infrequent_term_threshold,
                                intermediate_directory){

    setwd(intermediate_directory)

    cat("Currently working on combination",i,"of",nrow(choices),"\n")
    ptm <- proc.time()

    # need a conditional for removing stopwords
    if (choices$removeStopwords[i]) {
        # generate dfm
        if (choices$use_ngrams[i]) {
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i],
                ignoredFeatures = quanteda::stopwords(),
                ngrams = 1:3)
        } else {
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i],
                ignoredFeatures = quanteda::stopwords())
        }
    } else {
        # generate dfm
        if (choices$use_ngrams[i]) {
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i],
                ngrams = 1:3)
        } else {
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i])
        }
    }

    # remove infrequent terms
    if (choices$infrequent_terms[i]) {
        current_dfm <- remove_infrequent_terms(
            dfm_object = current_dfm,
            proportion_threshold = infrequent_term_threshold)
    }

    t2 <- proc.time() - ptm
    ret <- paste("Complete in:",t2[[3]],"seconds")

    # store the current dfm
    save(current_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))

    return(ret)
}
