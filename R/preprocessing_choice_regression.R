#' @title Preprocessing Choice Regressions
#' @description Assessing the effects of preprocessing decisions on an outcome
#' variable.
#'
#' @param Y A vector of length 128 (usually) containing a numeric outcome
#' variable. This should be the preText (or other) score for a particular
#' preprocessing specification.
#' @param choices A 128 x 7 data.frame produced by the `factorial_preprocessing()`
#' function and output in the `$choices` field.
#' @param dataset The name to be given to the data we are analyzing.
#' @param base_case_index An optional argument which removes a base case row from
#' the choices data before performing the regression.
#' @return A data.frame
#' @examples
#' \dontrun{
#' # *** note that this function is already called in the preText() function and
#' # its output is returned in the results.
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
#' # get regression results
#' reg_results <- preprocessing_choice_regression(
#'      preText_results$preText_scores$preText_score,
#'      preprocessed_documents$choices,
#'      dataset = "UK Manifestos",
#'      base_case_index = 128)
#' }
#' @export
preprocessing_choice_regression <- function(Y,
                                            choices,
                                            dataset = "UK",
                                            base_case_index = 128) {

    # get the appropriate response vectors and make datasets
    Y <- Y
    if(!is.null(base_case_index)) {
        choices <- choices[-base_case_index,]
    }
    DATA <- cbind(Y,choices)

    if (nrow(choices) < 127) {
        form <- "Y ~ removePunctuation + removeNumbers + lowercase + stem + removeStopwords + infrequent_terms"

        var_names <- c("Intercept", "Remove Punctuation", "Remove Numbers",
                       "Lowercase","Stemming", "Remove Stopwords",
                       "Remove Infrequent Terms")
    } else {
        form <- "Y ~ removePunctuation + removeNumbers + lowercase + stem + removeStopwords + infrequent_terms + use_ngrams"

        var_names <- c("Intercept", "Remove Punctuation", "Remove Numbers",
                       "Lowercase","Stemming", "Remove Stopwords",
                       "Remove Infrequent Terms",  "Use NGrams" )
    }

    fit <- lm(formula = form, data = DATA)
    cat("The R^2 for this model is:",summary(fit)$r.squared,"\n")
    sds <- summary(fit)$coefficients[,2]
    results1 <- cbind( quanteda::coef(fit),  sds)
    results1 <- as.data.frame(results1,
                              stringsAsFactors = FALSE)
    results <- cbind(results1,var_names)
    colnames(results) <- c("estimate", "sd", "variable")
    rownames(results) <- NULL

    if (nrow(choices) < 127) {
        results <- cbind(results, rep(dataset,7))
    } else {
        results <- cbind(results, rep(dataset,8))
    }

    colnames(results) <- c("Coefficient","SE","Variable","Model")

    return(results)
}
