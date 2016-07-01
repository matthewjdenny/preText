#' @title Ensemble Mantel Tests
#' @description Calculates Mantel test statistics for differences between
#' distance matrices for a list of distance matrices (one per preprocessing
#' method) supplied by the scaling_comparison() function.
#'
#' @param distance_matrices A list of document similarity matrices.
#' @param names Optional argument giving names for each preprocessing step.
#' @param permutations the number of permutations to be used in each Mantel
#' test. Defaults to 999.
#' @return A result list object where the first entry is a matrix summarizing
#' mantel test statististics. The fourth column of this matrix records the
#' difference between the test statistic and the 99th percentile of the null
#' distribution, and is preferred for comparing between runs. A positive value
#' indicates significance at atleast the 0.01 level, while a negative value
#' indciates insignificance. The second object in the list is a matrix of the
#' values described above. The third object is a list of all raw mantel results.
#' @export
mantel_comparison <- function(distance_matrices,
                              names = NULL,
                              permutations = 999){

    # get the number of distance matrices
    num_dms <- length(distance_matrices)

    # ceate data structures to store information
    result_list <- vector(mode = "list", length = num_dms*(num_dms - 1))
    mantel_matrix <- matrix(0, nrow = num_dms, ncol = num_dms)
    result_summary <- matrix(0, nrow = num_dms*(num_dms - 1)/2,ncol = 4)
    summary_counter <- 1
    list_counter <- 1

    colnames(result_summary) <- c("statistic",
                                  "p_value",
                                  "99th_pct_null_stat",
                                  "difference")

    if (!is.null(names)) {
        colnames(mantel_matrix) <- rownames(mantel_matrix) <- names
    }

    for (i in 1:num_dms) {
        cat("Currently working on preprocessing choice",i,"of",num_dms,"\n")
        ptm <- proc.time()
        # get the current focal distance matrix
        cur_dm <- distance_matrices[[i]]

        # now loop over all of the others
        for (j in 1:num_dms) {
            if (i != j){
                result <- vegan::mantel(distance_matrices[[i]],
                                        distance_matrices[[j]],
                                        permutations = permutations)
                if (j > i) {
                    result_summary[summary_counter,1] <- result$statistic
                    result_summary[summary_counter,2] <- result$signif
                    result_summary[summary_counter,3] <- as.numeric(quantile(result$perm,.99))
                    result_summary[summary_counter,4] <- as.numeric(result$statistic -
                        quantile(result$perm,.99))

                    # give things the right row names if they were provided
                    if (!is.null(names)) {
                        rownames(result_summary)[summary_counter] <-
                            paste(names[i],"<->",names[j], sep = "")
                    }

                    summary_counter <- summary_counter + 1
                }
                mantel_matrix[i,j] <- as.numeric(result$statistic -
                                                     quantile(result$perm,.99))

                result_list[[list_counter]] <- result

                # give things the right row names if they were provided
                if (!is.null(names)) {
                    names(result_list)[list_counter] <-
                        paste(names[i],"<->",names[j], sep = "")
                }

                list_counter <- list_counter + 1
            }
        }

        t2 <- proc.time() - ptm
        cat("Complete in:",t2[[3]],"seconds...\n")
    }

    return(list(summary = result_summary,
                mantel_difference_matrix = mantel_matrix,
                raw_results = result_list))

}
