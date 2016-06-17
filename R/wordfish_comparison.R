#' @title Wordfish Comparison.
#' @description Calculated Wordfish scores for a list of dfm objects with
#' temporal filtering.
#'
#' @param dfm_object_list A list of quanteda dfm() objects.
#' @param years A numeric vector giving the year for each document.
#' @param anchors A numeric vetor of length two used to anchor the wordfish
#' estimates. Defaults to c(1,24) which should work for the UK parlaiment docs.
#' @param proportion_threshold proportion of years a term must be included in
#' to be included in the Wordfish analysis.
#' @return A result list object
#' @export
wordfish_comparison <- function(dfm_object_list,
                            years,
                            anchors = c(1,24),
                            proportion_threshold = 1){

    # get the number of dfms
    num_dfms <- length(dfm_object_list)

    # ceate data structures to store information
    score_list <- vector(mode = "list", length = num_dfms)
    max_min <- matrix(0, nrow = num_dfms,ncol = 2)

    for (i in 1:num_dfms) {
        cat("Currently working on dfm",i,"of",num_dfms,"\n")
        ptm <- proc.time()
        # apply temporal filter
        dfm <- temporal_filter(dfm_object_list[[i]],
                               years,
                               proportion_threshold = proportion_threshold)
        # run wordfish
        result <- quanteda::textmodel_wordfish(dfm,dir = c(1,24))

        # create a summary data frame
        tp <- data.frame(document = dfm@Dimnames$docs[order(result@theta)],
                         score = result@theta[order(result@theta)],
                         stringsAsFactors = FALSE)

        # store everything
        score_list[[i]] <- tp
        max_min[i,] <- c(order(result@theta)[1], order(result@theta)[length(result@theta)])
        t2 <- proc.time() - ptm
        cat("Complete in:",t2[[3]],"seconds...\n")
    }

    return(list(summary_results = max_min,
                results_by_dfm = score_list))

}
