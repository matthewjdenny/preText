build_distance_objects <- function(cur_dfm, distance_method, dimensions) {
    if (distance_method == "cosine") {
        simil <- quanteda::textstat_simil(cur_dfm, method = distance_method)
    } else {
        simil <- proxy::simil(as.matrix(cur_dfm), method = distance_method)
    }
    simil <- as.matrix(simil)
    distances2 <- proxy::pr_simil2dist(simil)
    distances <- as.matrix(distances2)
    pos <- stats::cmdscale(distances, k = dimensions)
    out <- list('matrices' = distances,
                'objects' = proxy::as.dist(distances2),
                'positions' = pos)
    return(out)
}

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
#' @param cores The number of cores to be used for parallelization (optional).
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
                               verbose = TRUE,
                               cores = 1){

    if (cores > 1) { # multi-core
        if (verbose) {
            warning('verbose = TRUE only works with single-core computation.')
        }
        cl <- parallel::makeCluster(getOption("cl.cores", cores)) # start cluster
        tmp <- parallel::parLapplyLB(cl = cl,
                                     fun = build_distance_objects,
                                     X = dfm_object_list,
                                     distance_method = distance_method,
                                     dimensions = dimensions)
        distance_matrices <- lapply(tmp, function(x) x[['matrices']])
        distance_objects <- lapply(tmp, function(x) x[['objects']])
        scaled_positions <- lapply(tmp, function(x) x[['positions']])
        parallel::stopCluster(cl) # stop cluster
    } else { # single-core
        # get the number of dfms
        num_dfms <- length(dfm_object_list)

        # ceate data structures to store information
        distance_matrices <- vector(mode = "list", length = num_dfms)
        distance_objects <- vector(mode = "list", length = num_dfms)
        scaled_positions <- vector(mode = "list", length = num_dfms)

        for (i in 1:num_dfms) {
            if (verbose) {
                ptm <- proc.time()
                cat("Currently working on dfm",i,"of",num_dfms,"\n")
            }

            # compute
            tmp <- build_distance_objects(dfm_object_list[[i]], distance_method, dimensions)

            # store
            distance_matrices[[i]] <- tmp[['matrices']]
            distance_objects[[i]] <- tmp[['objects']]
            scaled_positions[[i]] <- tmp[['positions']]

            if (verbose) {
                t2 <- proc.time() - ptm
                cat("Complete in:",t2[[3]],"seconds...\n")
            }
        }
    }

    return(list(distance_matrices = distance_matrices,
                scaled_positions = scaled_positions,
                dist_objects = distance_objects))

}
