#' @title Regression Coefficient Plot
#' @description Easy plotting for multiple regression results on same axes.
#'
#' @param data A data.frame produced by the `preprocessing_choice_regression()`
#' function, or a list created by the `preText()` function.
#' @param text_size The size of the text to be displayed. Defaults to 1.
#' @param remove_intercept Logical indicating whether intercept coefficient should
#' be plotted. Defaults to FALSE.
#' @param title The title the user wishes to give the lot, which will be displayed
#' instead of the axis title.
#' @return A plot
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
#' # run preText
#' preText_results <- preText(
#'     preprocessed_documents,
#'     dataset_name = "Inaugural Speeches",
#'     distance_method = "cosine",
#'     num_comparisons = 100,
#'     verbose = TRUE)
#' # generate regression results
#' regression_coefficient_plot(preText_results,
#'                             remove_intercept = TRUE)
#' }
#' @export
regression_coefficient_plot <- function(data,
                                        text_size = 1,
                                        remove_intercept = FALSE,
                                        title = "") {

    if (class(data) == "list") {
        data <- data$regression_results
    }
    #define colors
    Model <- Variable <- Coefficient <- SE  <- NULL

    # if we are only using the one model, proceed as normal.

    if (remove_intercept) {
        data <- data[-which(data$Variable == "Intercept"),]
    }

    zp1 <- ggplot2::ggplot(data, ggplot2::aes(colour = Model)) +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::theme(axis.text = ggplot2::element_text(size = text_size))
    zp1 <- zp1 + ggplot2::geom_vline(xintercept = 0,
                                     colour = gray(1/2),
                                     lty = 2)
    zp1 <- zp1 + ggplot2::geom_segment(
        ggplot2::aes(y = Variable,
                     yend = Variable,
                     x = Coefficient - SE*(-qnorm((1 - 0.95)/2)),
                     xend = Coefficient + SE*(-qnorm((1 - 0.95)/2))),
        lwd = 1)
    zp1 <- zp1 + ggplot2::geom_point(
        ggplot2::aes(y = Variable,
                     x = Coefficient),
        lwd = 2,
        shape = 21, fill = "BLACK")

    zp1 <- zp1  + ggplot2::theme_bw() +
        ggplot2::facet_grid(~ Model, scales = "free_x") +
        ggplot2::theme(legend.position = "none",
                       axis.text.x=ggplot2::element_text(angle=-45, hjust=0),
                       axis.title.y=ggplot2::element_text(angle=-90, size=20, face="bold")) +
        ggplot2::ylab(title) +
        ggplot2::xlab("Regression Coefficient")
    #ggplot2::xlab("Normalized Effect on Rank Difference")


    print(zp1)
}
