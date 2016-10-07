#' @title Comparison of dfms using N-dimensional scaling, with a test
#' for difference from the mean dfm scaled position.
#' @description Scale each dfm into a N-d space and test for outliers.
#'
#' @param scaling_results A list object produced by the `scaling_comparison()`
#' function.
#' @param labels A character vector with labels for each dfm. This can be
#' extracted from the `$labels` field of the output from the
#' `factorial_preprocessing()` function.
#' @param dimensions The number of dimensions to be used by the multidimensional
#' scaling algorithm. Defaults to 2.
#' @param distance_method The method that should be used for calculating
#' distances between dfms. Defaults to "cosine".
#' @param method Should the raw distances or scaled document positions be used
#' for scaling? Can be one of c("distances","positions"), defaults to
#' "distances".
#' @param return_positions Logical indicating whether dfm positions should be
#' returned as a data.frame. Defaults to FALSE
#' @return A result list object, or a plot, or both.
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
#' # now perform the scaling test
#' dfm_scaling_test(scaling_results,
#'                  labels = preprocessed_documents$labels)
#' }
#' @export
dfm_scaling_test <- function(scaling_results,
                             labels,
                             dimensions = 2,
                             distance_method = "cosine",
                             method = c("distances","positions"),
                             return_positions = FALSE){

    method <- method[1]

    if (method == "distances") {
        # extract similarity matrix list
        dist_mat_list <- scaling_results$distance_matrices

        # get the number of dfms
        num_dfms <- length(dist_mat_list)

        ncols <- (length(lower.tri(dist_mat_list[[1]])) -
                      nrow(dist_mat_list[[1]]))/2
        # ceate data structures to store information
        document_distances <- matrix(0,
                                     ncol = ncols,
                                     nrow = num_dfms)

        # populate the distance comparison matrix
        for (i in 1:num_dfms) {
            document_distances[i,] <- dist_mat_list[[i]][
                lower.tri(dist_mat_list[[i]])]
        }

        # give descriptive row names
        rownames(document_distances) <- labels

        document_distances <- apply(document_distances,2,rev)
        # now generate a distance matrix on this dataset
        dfm_distances <- proxy::dist(document_distances,
                                       method = distance_method,
                                       diag = FALSE,
                                       by_rows = TRUE)
        dfm_distances <- as.matrix(dfm_distances)

        # now scale this distance
        pos <- stats::cmdscale(dfm_distances, k = dimensions)
        rownames(pos)[1] <- " "
        dists <- dfm_distances
    } else {
        # now do the same thing but on the procrustes transformed scaled document
        # positions
        positions_list <- scaling_results$scaled_positions
        anchor_positions <- positions_list[[1]]
        anchor_positions <- anchor_positions[order(rownames(anchor_positions)),]

        document_positions <- matrix(0,
                                     ncol = length(c(anchor_positions)),
                                     nrow = num_dfms)

        document_positions[1,] <- c(anchor_positions)
        # rotate the other positions and put them together in a big data.frame
        for (i in 2:num_dfms) {
            cur_pos <- positions_list[[i]][order(rownames(positions_list[[i]])),]
            cur <- vegan::procrustes(anchor_positions,
                                     cur_pos,
                                     scale = F)$Yrot

            document_positions[i,] <- c(cur)
        }

        # set rownames before reversing
        rownames(document_positions) <- labels

        # reverse order to have no preprocessing first
        document_positions <- apply(document_positions,2,rev)

        # now generate a distance matrix on this dataset
        dfm_distances2 <- proxy::dist(document_positions,
                                        method = distance_method,
                                        diag = FALSE,
                                        by_rows = TRUE)
        dfm_distances2 <- as.matrix(dfm_distances2)

        # now scale this distance
        pos <- stats::cmdscale(dfm_distances2, k = dimensions)
        rownames(pos)[1] <- " "
        dists <- dfm_distances2
    }


    # now we calculate mean, distance from mean, distribution of distances from
    # mean, t-test on distances, and draw circle in plot.

    mean_values <- rep(0,dimensions)
    for(i in 1:dimensions) {
        mean_values[i] <- mean(pos[,i])
    }

    # now calculate euclidean distances from mean value
    euclidean_distance_from_mean <- rep(0, num_dfms)
    for (i in 1:num_dfms) {
       dist <- 0
       for (j in 1:dimensions) {
           dist <- dist + (pos[i,j] - mean_values[j])^2
       }
       euclidean_distance_from_mean[i] <- sqrt(dist)
    }

    distance_sd <- sd(euclidean_distance_from_mean)

    significance_threshold <- 1.96 * distance_sd

    remove <- which(euclidean_distance_from_mean < significance_threshold)

    UMASS_BLUE <- rgb(51,51,153,195,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,195,maxColorValue = 255)

    plot(pos[,1:2],
         pch = 19,
         col = UMASS_BLUE,
         xlab = "Dimension 1",
         ylab = "Dimension 2",
         main = "Document-Term Matrix Scaled Positions")

    # now draw circle for significance threshold
    r <- significance_threshold
    nseg <- 360
    x.cent <- mean_values[1]
    y.cent <- mean_values[2]
    xx <- x.cent + r*cos( seq(0,2*pi, length.out=nseg) )
    yy <- y.cent + r*sin( seq(0,2*pi, length.out=nseg) )
    lines(xx, yy, col=UMASS_RED)

    # now add in names for points outside of significance threshold
    to_plot <- rownames(pos)
    if (length(remove) > 0) {
        to_plot[remove] <- " "
    }
    text(pos[,1:2],to_plot)

    # create dataframe to return
    different_from_mean <- rep(TRUE,num_dfms)
    if (length(remove) > 0) {
        different_from_mean[remove] <- FALSE
    }
    dfm_positions <- pos
    dfm_positions <- as.data.frame(dfm_positions)
    colnames(dfm_positions) <- paste("Dim_",1:ncol(dfm_positions),sep = "")
    dfm_positions <- cbind(dfm_positions,
                           euclidean_distance_from_mean,
                           different_from_mean)

    if (return_positions) {
        return(list(dfm_positions = dfm_positions,
                    dfm_distances = dists))
    }
}

