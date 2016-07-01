#' @title Optimal Topic Model k Comparison
#' @description Calculate the optimal number of topics for LDA using perplexity
#' for each dfm.
#'
#' @param dfm_object_list A list of quanteda dfm() objects.
#' @param interval The maximum and minimum number of topics to be searched over
#' defaults to 2 for minimum and 500 for maximum.
#' @param names optinal names for each dfm to make downstream interpretation
#' easier. Defaults to NULL.
#' @param parallel Logical indicating whether model fitting should be
#' performed in parallel. Defualts to FALSE.
#' @param cores Defualts to 1, can be set to any number less than or equal to
#' the number of cores on one's computer.
#' @return A vector containing the optiaml k for each dfm.
#' @export
optimal_k_comparison <- function(dfm_object_list,
                                 interval = c(2, 500),
                                 names  = NULL,
                                 parallel = FALSE,
                                 cores = 1){

    # get the number of dfms
    num_dfms <- length(dfm_object_list)

    # ceate data structures to store information
    optimal_k <- rep(0, num_dfms)

    if (parallel) {
        cat("Finding optimal K from LDA",num_dfms,"different ways on",
            cores,"cores. This may take a while...\n")
        cl <- parallel::makeCluster(getOption("cl.cores", cores))

        optimal_k <- parallel::clusterApplyLB(
            cl = cl,
            x = 1:num_dfms,
            fun = find_optimal_number_of_topics,
            dfm_list = dfm_object_list,
            interval = interval)
        # stop the cluster when we are done
        parallel::stopCluster(cl)
    } else {
        for (i in 1:num_dfms) {
            cat("Currently working on dfm",i,"of",num_dfms,"\n")
            ptm <- proc.time()
            # apply temporal filter

            optimal_k[i] <- find_optimal_number_of_topics(i,
                                                          dfm_object_list,
                                                          interval = interval)

            t2 <- proc.time() - ptm
            cat("Complete in:",t2[[3]],"seconds...\n")
        }
    }

    if (!is.null(names)) {
        names(optimal_k) <- names
    }

    return(optimal_k)

}



