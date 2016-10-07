#' @title Topic Top-Terms Novelty Score
#' @description Calculates the novelty score for a character matrix displaying
#' topic top-terms for all topics.
#'
#' @param top_terms_matrix A character matrix or data.frame containing top
#' terms for all topics.
#' @param row_range Optional argument specifying range of rows to keep. This is
#' useful if we are only interested in the top 10 or 20 terms, but we have a
#' matrix with the top 50 terms for each topic.
#' @return A novelty score.
#' @examples
#' \dontrun{
#' set.seed(12345)
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
#' cross_validation_splits <- 10
#' # create 10 test/train splits
#' train_inds <- vector(mode = "list", length = cross_validation_splits)
#' test_inds <- vector(mode = "list", length = cross_validation_splits)
#' # sample CV indices
#' for (i in 1:cross_validation_splits) {
#'     test <- sample(1:length(UK_Manifestos),
#'                    size = round(length(UK_Manifestos)/5),
#'                    replace = FALSE)
#'     train <- 1:length(UK_Manifestos)
#'     for (j in 1:length(test)) {
#'         train <- train[-which(train == test[j])]
#'     }
#'     train_inds[[i]] <- train
#'     test_inds[[i]] <- test
#' }
#' # get the optimal number of topics (this will take a very long time):
#' optimal_k <- optimal_k_comparison(
#'      train_inds,
#'      test_inds,
#'      preprocessed_documents$dfm_list,
#'      topics = c(25,50,75,100,125,150,175,200),
#'      names  = preprocessed_documents$labels)
#' # run a topic model with the optimal number of topics for each preproc. spec.
#' top_terms_list <- vector(mode = "list", length = 128)
#' for (i in 1:128) {
#'      fit <- topicmodels::LDA(quanteda::convert(preprocessed_documents$dfm_list[[i]],
#'                                                to = "topicmodels"),
#'                              k = optimal_k[i])
#'      # extract out top 20 terms for each topic
#'      top_terms <- terms(fit,20)
#'      top_terms_list[[i]] <- top_terms
#' }
#' # calculate novelty score
#' topic_novelty_score(top_terms_list[[1]])
#' }
#' @export
topic_novelty_score <- function(top_terms_matrix,
                          row_range = NULL) {
    if (!is.null(row_range)) {
        top_terms_matrix <- top_terms_matrix[,row_range]
    }
    top_terms_matrix_vec <- c(top_terms_matrix)
    top_terms_matrix_vec <- toLower(top_terms_matrix_vec)
    scores <- rep(0, nrow(top_terms_matrix)*ncol(top_terms_matrix))
    for(i in 1:length(top_terms_matrix_vec)) {
        scores[i] <- 1/sum(grepl(top_terms_matrix_vec[i],
                                 top_terms_matrix_vec, fixed = TRUE))
    }
    score <- sum(scores)/length(top_terms_matrix_vec)
    return(score)
}