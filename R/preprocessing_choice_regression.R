#' @title Preprocessing Choice Regressions
#' @description Assessing the effects of preprocessing decisions on an outcome
#' variable.
#'
#' @param  A vector of length 128 (usually) containing a numeric outcome
#' variable.
#' @param choices A 128 x 7 data.frame produced by the factorial_preprocessing
#' function.
#' @param dataset The name to be given to the data we are analyzing.
#' @param base_case_index An optional argument whic removes a base case row from
#' the choices data before performing the regrerssion.
#' @return A data.frame
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

    form <- "Y ~ removePunctuation + removeNumbers + lowercase + stem + removeStopwords + infrequent_terms + use_ngrams"

    var_names <- c("Intercept", "Remove Punctuation", "Remove Numbers",
                   "Lowercase","Stemming", "Remove Stopwords",
                   "Remove Infrequent Terms",  "Use NGrams" )

    fit <- lm(formula = form, data = DATA)
    sds <- summary(fit)$coefficients[,2]
    results1 <- cbind( coef(fit),  sds)
    results1 <- as.data.frame(results1,
                              stringsAsFactors = FALSE)
    results <- cbind(results1,var_names)
    colnames(results) <- c("estimate", "sd", "variable")
    rownames(results) <- NULL

    results <- cbind(results, rep(dataset,8))
    colnames(results) <- c("Coefficient","SE","Variable","Model")

    return(results)
}
