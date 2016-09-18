parallel_rank_test <- function(x,
                               distance_matrices,
                               baseline,
                               baseline_index,
                               num_comparisons) {

    num_dms <- length(distance_matrices)
    i <- x
    # figures out which entry in the distance matrix has the largest difference
    # from it to the base case entry.
    get_max_difference <- function (dist_matrix,
                                    baseline) {
        temp <- abs(dist_matrix - baseline)
        temp <- temp[lower.tri(temp)]
        ordering <- order(temp, decreasing = TRUE)
        return(ordering)
    }

    # find out the average rank of this distance difference in all of the other
    # preprocessing steps. Returns the average (absolute) rank difference
    compare_top_difference <- function(distance_matrices,
                                       baseline,
                                       i,
                                       baseline_index,
                                       most_different_index,
                                       place) {
        # create indices to loop through
        inds <- 1:length(distance_matrices)
        inds <- inds[-c(baseline_index, i)]

        rank_of_top_difference <- rep(0,length(inds))
        counter <- 1
        for (j in inds) {

            dist_mat <- distance_matrices[[j]]
            comparison_ordering <- get_max_difference(dist_mat, baseline)
            rank_val <- which(comparison_ordering == most_different_index)
            rank_val <- abs(rank_val - place)
            rank_of_top_difference[counter] <- rank_val
            counter <- counter + 1
        }
        rank_of_top_difference <- rank_of_top_difference/length(lower.tri(dist_mat))
        average_rank <- mean(rank_of_top_difference)

        return(list(ave_diff = average_rank,
                    ave_diff_SE = sd(rank_of_top_difference)))
    }

    score <- 0
    if (i != baseline_index) {
        dist_matrix <- distance_matrices[[i]]
        ordering <- get_max_difference(dist_matrix,
                                       baseline)

        samp <- 1:num_comparisons
        #samp <- sample(1:length(ordering),num_comparisons)
        cur_scores <- rep(0,num_comparisons)
        cur_score_SE <- rep(0,num_comparisons)
        for (k in 1:num_comparisons) {
            most_different_index <- ordering[samp[k]]
            result <- compare_top_difference(distance_matrices,
                                             baseline,
                                             i,
                                             baseline_index,
                                             most_different_index,
                                             place = samp[k])
            cur_scores[k] <- result$ave_diff
            cur_score_SE[k] <- result$ave_diff_SE
        }
        score <- mean(cur_scores)
    }

    return(score)
}
