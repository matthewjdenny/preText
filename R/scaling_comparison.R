#' @title Scaling Comparison.
#' @description Scale each dfm and return a list of similarity matrices and
#' scaled document positions.
#'
#' @param dfm_object_list A list of quanteda dfm() objects.
#' @param dimensions THe number of dimensions to be used by the multidimensional
#' scaling algorithm. Defualts to 2.
#' @param distance_method The method that should be used for calculating
#' document distances. Defualts to "cosine".
#' @return A result list object.
#' @export
scaling_comparison <- function(dfm_object_list,
                               dimensions = 2,
                               distance_method = "cosine"){

    # get the number of dfms
    num_dfms <- length(dfm_object_list)

    # ceate data structures to store information
    distance_matrices <- vector(mode = "list", length = num_dfms)
    distance_objects <- vector(mode = "list", length = num_dfms)
    scaled_positions <- vector(mode = "list", length = num_dfms)

    for (i in 1:num_dfms) {
        cat("Currently working on dfm",i,"of",num_dfms,"\n")
        ptm <- proc.time()
        # apply temporal filter
        cur_dfm <- dfm_object_list[[i]]

        # calculate the document similarity matrix
        if (distance_method == "cosine") {
            simil <- quanteda::similarity(cur_dfm, method = distance_method)
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
        cat("Complete in:",t2[[3]],"seconds...\n")
    }

    return(list(distance_matrices = distance_matrices,
                scaled_positions = scaled_positions,
                dist_objects = distance_objects))

}

