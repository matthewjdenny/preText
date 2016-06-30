#' @title A function to perform factorial preprocessing of a corpus of texts
#' into document-frequency matrices.
#' @description Preprocesses a corpus of texts into a document-frequency matrix
#' in thirty-two different ways.
#'
#' @param text A vector of strings (one per document) or Quanteda corpus object
#' from which we wish to form a document-term matrix.
#' @param use_ngrams Option to extract 1,2, and 3-grams from the text as another
#' potential preprocessing step. Defualts to TRUE.
#' @param filter_infrequent_terms Option to remove all terms appearing in less
#' than 1 percent of documents as a preprocessing decision. Defaults to TRUE.
#' @param infrequent_term_threshold A proportion threshold at which infrequent
#' terms are to be filtered. Defaults to 0.01 (terms that appear in less than
#' 1 percent of documents).
#' @param parallel Logical indicating whether factorial prerpocessing shoudl be
#' performed in parallel. Defualts to FALSE.
#' @param cores Defualts to 1, can be set to any number less than or equal to
#' the number of cores on one's computer.
#' @return A list object containing permutations of the document-term matrix.
#' @export
factorial_preprocessing <- function(text,
                                    use_ngrams = TRUE,
                                    filter_infrequent_terms = TRUE,
                                    infrequent_term_threshold = 0.01,
                                    parallel = FALSE,
                                    cores = 1){
    # check to see if input is a corpus object. If it is, extract the texts
    if (class(text)[1] == "corpus") {
        text <- quanteda::texts(text)
    }

    # now make sure we have a character vector
    if (class(text)[1] != "character") {
        stop("You must provide either a character vector of strings (one per document, or a quanteda corpus object.")
    }

    # create a data.frame with factorial combinations of all choices.
    if (use_ngrams) {
        if (filter_infrequent_terms) {
            choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                                   removeNumbers = c(TRUE,FALSE),
                                                   lowercase = c(TRUE,FALSE),
                                                   stem = c(TRUE,FALSE),
                                                   removeStopwords = c(TRUE,FALSE),
                                                   infrequent_terms = c(TRUE, FALSE),
                                                   use_ngrams = c(TRUE, FALSE))))
        } else {
            choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                                   removeNumbers = c(TRUE,FALSE),
                                                   lowercase = c(TRUE,FALSE),
                                                   stem = c(TRUE,FALSE),
                                                   removeStopwords = c(TRUE,FALSE),
                                                   infrequent_terms = c(FALSE),
                                                   use_ngrams = c(TRUE, FALSE))))
        }
    } else {
        if (filter_infrequent_terms) {
            choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                                   removeNumbers = c(TRUE,FALSE),
                                                   lowercase = c(TRUE,FALSE),
                                                   stem = c(TRUE,FALSE),
                                                   removeStopwords = c(TRUE,FALSE),
                                                   infrequent_terms = c(TRUE, FALSE),
                                                   use_ngrams = c(FALSE))))
        } else {
            choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                                   removeNumbers = c(TRUE,FALSE),
                                                   lowercase = c(TRUE,FALSE),
                                                   stem = c(TRUE,FALSE),
                                                   removeStopwords = c(TRUE,FALSE),
                                                   infrequent_terms = c(FALSE),
                                                   use_ngrams = c(FALSE))))
        }
    }

    # create a list object in which to store the different dfm's
    dfm_list <- vector(mode = "list", length = nrow(choices))

    if (parallel) {
        cat("Preprocessing documents",nrow(choices),"different ways on",
            cores,"cores. This may take a while...\n")
        cl <- parallel::makeCluster(getOption("cl.cores", cores))

        dfm_list <- parallel::clusterApplyLB(
            cl = cl,
            x = 1:nrow(choices),
            fun = parallel_preprocess,
            text = text,
            infrequent_term_threshold = infrequent_term_threshold)
        # stop the cluster when we are done
        parallel::stopCluster(cl)
    } else {
        # loop over different preprocessing decisions
        for (i in 1:nrow(choices)) {
            cat("Currently working on combination",i,"of",nrow(choices),"\n")
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

            # store the current dfm
            dfm_list[[i]] <- current_dfm
        }
    }


    # combine metadata and dfm list and return
    return_list <- list(choices = choices,
                        dfm_list = dfm_list)

    return(return_list)
}

