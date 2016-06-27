#' @title Scaling Comparison.
#' @description Scale each dfm and return a list of similarity matrices and
#' scaled document positions.
#'
#' @param dfm_object_list A list of quanteda dfm() objects.
#' @param dimensions THe number of dimensions to be used by the multidimensional
#' scaling algorithm. Defualts to 2.
#' @param similarity_method The method that should be used for calculating
#' document similarities. Defualts to "cosine".
#' @return A result list object.
#' @export
scaling_comparison <- function(dfm_object_list,
                               dimensions = 2,
                               similarity_method = "cosine"){

    # get the number of dfms
    num_dfms <- length(dfm_object_list)

    # ceate data structures to store information
    similarity_matrices <- vector(mode = "list", length = num_dfms)
    scaled_positions <- vector(mode = "list", length = num_dfms)

    for (i in 1:num_dfms) {
        cat("Currently working on dfm",i,"of",num_dfms,"\n")
        ptm <- proc.time()
        # apply temporal filter
        cur_dfm <- dfm_object_list[[i]]

        # calculate the document similarity matrix
        simil <- quanteda::similarity(cur_dfm, method = similarity_method)
        sim <- as.matrix(simil)

        # get positions for each document in 2d space
        pos <- stats::cmdscale(sim, k = dimensions)

        # store everything
        similarity_matrices[[i]] <- sim
        scaled_positions[[i]] <- pos

        t2 <- proc.time() - ptm
        cat("Complete in:",t2[[3]],"seconds...\n")
    }

    return(list(similarity_matrices = similarity_matrices,
                scaled_positions = scaled_positions))

}

