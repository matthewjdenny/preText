parallel_preprocess <- function(i,
                                choices,
                                text,
                                infrequent_term_threshold,
                                intermediate_directory,
                                verbose,
                                save_intermediate = FALSE){

    setwd(intermediate_directory)

    if (verbose) {
        cat("Currently working on combination",i,"of",nrow(choices),"\n")
    }
    ptm <- proc.time()

    # stopword and ngrams settings
    if (choices$removeStopwords[i]) {
        sw <- quanteda::stopwords()
    } else {
        sw <- NULL
    }
    if (choices$use_ngrams[i]) {
        ng <- 1:3
    } else {
        ng <- 1
    }

    # create dfm
    current_dfm <- quanteda::dfm(text, 
                                 tolower = choices$lowercase[i],
                                 stem = choices$stem[i],
                                 remove_punct = choices$removePunctuation[i],
                                 remove_numbers = choices$removeNumbers[i],
                                 remove = sw,
                                 ngrams = ng)

    # remove infrequent terms
    if (choices$infrequent_terms[i]) {
        out <- quanteda::dfm_trim(current_dfm, 
                                  min_docfreq = infrequent_term_threshold,
                                  verbose = FALSE) # otherwise prints annoying message when no features removed
    }

    t2 <- proc.time() - ptm
    ret <- paste("Complete in:",t2[[3]],"seconds")


    # store the current dfm
    if (save_intermediate) {
        save(current_dfm, file = paste("intermediate_dfm_",i,".Rdata",sep = ""))
        out <- ret
    } else {
        out <- current_dfm
    }

    return(out)
}
