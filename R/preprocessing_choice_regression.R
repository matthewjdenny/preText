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