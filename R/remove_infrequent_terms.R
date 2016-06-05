#' @title Remove sparse terms from quanteda dfm.
#' @description Removes terms appearing in less than a specifiec percentage of
#' documents in a corpus from a dfm.
#'
#' @param dfm_object A quanteda dfm() object.
#' @param proportion_threshold proportion of documents a term must be included in
#' to be included in the dfm.
#' @return A reduced dfm.
#' @export
remove_infrequent_terms <- function(dfm_object,
                                    proportion_threshold = 0.01){

    # determine the number of documents a term must appear in to be kept
    threshold <- ceiling(proportion_threshold * nrow(dfm_object))

    # create a temporary dfm object
    temp_dfm <- dfm_object

    #set word counts to 1
    temp_dfm@x <- rep(1,length(temp_dfm@x))

    # get column sums
    doc_counts <- quanteda::colSums(temp_dfm)

    # determine which column
    remove <- as.numeric(which(doc_counts < threshold))
    cat("Removing",length(remove),"of",ncol(dfm_object),
        "total terms that appeared in less than",threshold,"documents.\n")

    if (length(remove) > 0) {
        # remove the low frequency terms
        dfm_object <- dfm_object[,-remove]
    }

    #return the dfm_object
    return(dfm_object)
}
