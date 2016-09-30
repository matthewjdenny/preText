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