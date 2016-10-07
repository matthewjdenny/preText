#' @title Ensemble Mantel Tests
#' @description Calculates Mantel test statistics for differences between
#' distance matrices for a list of distance matrices (one per preprocessing
#' method) supplied by the scaling_comparison() function to a base case --
#' (usually the no-preprocessing specification).
#'
#' @param distance_matrices A list of document distance matrices from th3
#' `$distance_matrices` field of the output from the `scaling_comparison()`
#' function.
#' @param names Optional argument giving names for each preprocessing step.
#' @param permutations The number of permutations to be used in each Mantel
#' test. Defaults to 1000.
#' @param base_dfm_index Which dfm should be used as a base case for comparing
#' r statistics with bootstrapped confidence intervals.
#' @param text_size The `cex` for the x-labels, defaults to 1.
#' @param return_values Logical indicating whether test statistics and
#' confidence bounds should be returned as a data.frame or not. Defaults to FALSE.
#' @return A data.frame with mantel statistics and 95 percent confidence
#' intervals comparing all other preprocessing choices to base case, and/or a
#' plot of confidence intervals.
#' @examples
#' \dontrun{
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
#' # run mantel comparison to base and plot
#' mantel_comparison_to_base(scaling_results$distance_matrices,
#'                           names = preprocessed_documents$labels,
#'                           permutations = 1000)
#' }
#' @export
mantel_comparison_to_base <- function(distance_matrices,
                                      names = NULL,
                                      permutations = 1000,
                                      base_dfm_index = 128,
                                      text_size = 1,
                                      return_values = FALSE){

    Coefficient_Type <- Variable <- Coefficient <- SE <- NULL
    # get the number of distance matrices
    num_dms <- length(distance_matrices)

    cat("Currently calculating mantel distance confidence intevals\n")
    ptm <- proc.time()

    summary_counter <- 1
    result_summary2 <- matrix(0, nrow = (num_dms - 1),ncol = 3)
    rownames(result_summary2) <- as.character(1:(num_dms - 1))
    colnames(result_summary2) <- c("statistic",
                                   "lower_limit",
                                   "upper_limit")
    anchor_dfm <- distance_matrices[[base_dfm_index]]
    for (i in 1:num_dms) {

        # get the current focal distance matrix
        cur_dm <- distance_matrices[[i]]

        if (i != base_dfm_index) {
            result <- ecodist::mantel(anchor_dfm ~ cur_dm,
                                      nperm = permutations)

            result_summary2[summary_counter,1] <- as.numeric(result[1])
            result_summary2[summary_counter,2] <- as.numeric(result[5])
            result_summary2[summary_counter,3] <- as.numeric(result[6])

            # give things the right row names if they were provided
            if (!is.null(names)) {
                rownames(result_summary2)[summary_counter] <-
                    paste(names[i],"<->",names[base_dfm_index], sep = "")
            }

            summary_counter <- summary_counter + 1
        }


    }

    t2 <- proc.time() - ptm
    cat("Complete in:",t2[[3]],"seconds...\n")

    result_summary2 <- result_summary2[order(result_summary2[,1]),]

    data <- data.frame(Coefficient = result_summary2[,1],
                       SE = (result_summary2[,1]- result_summary2[,2])/1.96,
                       Coefficient_Type = "Mantel_Statistic",
                       Variable = rownames(result_summary2),
                       stringsAsFactors = FALSE)

    data$Variable <- factor(data$Variable,
                            levels = data$Variable[1:nrow(data)])

    UMASS_BLUE <- rgb(51,51,153,195,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,195,maxColorValue = 255)

    zp1 <- ggplot2::ggplot(data, ggplot2::aes(colour = Coefficient_Type)) +
        ggplot2::scale_color_manual(values = UMASS_BLUE) +
        ggplot2::theme(axis.text = ggplot2::element_text(size = text_size))

    zp1 <- zp1 + ggplot2::geom_hline(yintercept = 0,
                                 colour = gray(1/2),
                                 lty = 2)
    zp1 <- zp1 + ggplot2::geom_linerange( ggplot2::aes(x = Variable,
                                ymin = Coefficient - SE*(-qnorm((1 - 0.9)/2)),
                                ymax = Coefficient + SE*(-qnorm((1 - 0.9)/2))),
                                lwd = 1,
                                position = ggplot2::position_dodge(width = 1/2))
    zp1 <- zp1 + ggplot2::geom_pointrange(ggplot2::aes(x = Variable,
                                y = Coefficient,
                                ymin = Coefficient - SE*(-qnorm((1 - 0.95)/2)),
                                ymax = Coefficient + SE*(-qnorm((1 - 0.95)/2))),
                                lwd = 1/2,
                                position = ggplot2::position_dodge(width = 1/2),
                                shape = 21, fill = "WHITE")
    zp1 <- zp1  + ggplot2::theme_bw() +
        ggplot2::coord_flip() +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ylab("Mantel Test Statistic")

    print(zp1)

    if (return_values) {
        return(result_summary2)
    }
}
