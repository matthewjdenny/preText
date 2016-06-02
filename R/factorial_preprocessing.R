#' @title A function to perform factorial preprocessing of a corpus of texts
#' into document-frequency matrices.
#' @description Preprocesses a corpus of texts into a document-frequency matrix
#' in thirty-two different ways.
#'
#' @param text A vector of strings (one per document) or Quanteda corpus object
#' from which we wish to form a document-term matrix.
#' @return A list object containing permutations of the document-term matrix.
#' @export
factorial_preprocessing <- function(text){
    # check to see if input is a corpus object. If it is, extract the texts
    if (class(text)[1] == "corpus") {
        text <- quanteda::texts(text)
    }

    # now make sure we have a character vector
    if (class(text)[1] != "character") {
        stop("You must provide either a character vector of strings (one per document, or a quanteda corpus object.")
    }

    # create a data.frame with factorial combinations of all choices
    choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                           removeNumbers = c(TRUE,FALSE),
                                           lowercase = c(TRUE,FALSE),
                                           stem = c(TRUE,FALSE),
                                           removeStopwords = c(TRUE,FALSE))))

    # create a list object in which to store the different dfm's
    dfm_list <- vector(mode = "list", length = nrow(choices))

    # loop over different preprocessing decisions
    for (i in 1:nrow(choices)) {
        cat("Currently working on combination",i,"of",nrow(choices),"\n")
        # need a conditional for removing stopwords
        if (choices$removeStopwords[i]) {
            # generate dfm
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i],
                ignoredFreatures = quanteda::stopwords())
        } else {
            # generate dfm
            current_dfm <- quanteda::dfm(
                text,
                removePunct = choices$removePunctuation[i],
                removeNumbers = choices$removeNumbers[i],
                toLower = choices$lowercase[i],
                stem = choices$stem[i])
        }

        dfm_list[[i]] <- current_dfm
    }

    # combine metadata and dfm list and return
    return_list <- list(choices = choices,
                        dfm_list = dfm_list)

    return(return_list)
}

