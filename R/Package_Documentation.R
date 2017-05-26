#' preText: Diagnostics to Assess The Effects of Text Preprocessing Decisions
#'
#' @section preText functions:
#' To use this package, You will first want to check out the
#' factorial_preprocessing() function which will take raw data and transform it
#' into document-frequency matrices using a factorial design and 6-7 different
#' preprocessing decisions. The next step in most applications will be to run
#' the preText() function, which will generate preText scores for each preprocessing
#' specification. These can then be fed to the preText_score_plot() and
#' regression_coefficient_plot() functions to generate interpretable output.
#' For more information on additional functions check out the GitHub README for
#' this package (https://github.com/matthewjdenny/preText) or the "getting
#' started" vignette by typing `vignette("getting_started_with_preText")` into
#' the console.
#'
#' @docType package
#' @name preText
NULL
#> NULL

#' @import quanteda
NULL

#' @importFrom grDevices rgb gray
NULL

#' @importFrom stats quantile lm qnorm sd
NULL

#' @importFrom graphics lines plot text
NULL

