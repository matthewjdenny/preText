#' @title Scaling Comparison.
#' @description Scale each dfm and return a list of distance matrices and
#' scaled document positions.
#'
#' @param dfm_object_list A list of quanteda dfm objects returned in the
#' `$dfm_list$ field of the output from the `factorial_preprocessing()` function.
#' @param dimensions The number of dimensions to be used by the multidimensional
#' scaling algorithm. Defaults to 2.
#' @param distance_method The method that should be used for calculating
#' document distances. Defaults to "cosine".
#' @param verbose Logical indicating whether more information should be printed
#' to the screen to let the user know about progress. Defaults
#' to TRUE.
#' @return A result list object.
#' @examples
#' \dontrun{
#' # *** This function is used automatically inside of the preText() function.
#' # load the package
#' library(preText)
#' # load in the data
#' data("UK_Manifestos")
#' # preprocess data
#' preprocessed_documents <- factorial_preprocessing(
#'     UK_Manifestos,
#'     use_ngrams = TRUE,
#'     infrequent_term_threshold = 0.02,
#'     verbose = TRUE)
#' # scale documents
#' scaling_results <- scaling_comparison(preprocessed_documents$dfm_list,
#'                                       dimensions = 2,
#'                                       distance_method = "cosine",
#'                                       verbose = TRUE)
#' }
#' @export
scaling_comparison <- function(dfm_object_list,
                               dimensions = 2,
                               distance_method = "cosine",
                               verbose = TRUE){

    # get the number of dfms
    num_dfms <- length(dfm_object_list)

    # ceate data structures to store information
    distance_matrices <- vector(mode = "list", length = num_dfms)
    distance_objects <- vector(mode = "list", length = num_dfms)
    scaled_positions <- vector(mode = "list", length = num_dfms)

    for (i in 1:num_dfms) {
        if (verbose) {
            cat("Currently working on dfm",i,"of",num_dfms,"\n")
        }
        ptm <- proc.time()
        # apply temporal filter
        cur_dfm <- dfm_object_list[[i]]

        # calculate the document similarity matrix
        if (distance_method == "cosine") {
            simil <- quanteda::textstat_simil(cur_dfm, method = distance_method)
        } else {
            simil <- proxy::simil(as.matrix(cur_dfm), method = distance_method)
        }
        simil <- as.matrix(simil)
        distances2 <- proxy::pr_simil2dist(simil)
        distances <- as.matrix(distances2)

        # get positions for each document in 2d space
        pos <- stats::cmdscale(distances, k = dimensions)

        # store everything
        distance_matrices[[i]] <- distances
        distance_objects[[i]] <- proxy::as.dist(distances2)
        scaled_positions[[i]] <- pos

        t2 <- proc.time() - ptm
        if (verbose) {
            cat("Complete in:",t2[[3]],"seconds...\n")
        }
    }

    return(list(distance_matrices = distance_matrices,
                scaled_positions = scaled_positions,
                dist_objects = distance_objects))

}

