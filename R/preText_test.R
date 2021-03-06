#' @title preText Test
#' @description calculates preText scores for each preprocessing specification.
#'
#' @param distance_matrices A list of document distance matrices generated by the
#' `scaling_comparison()` function and returned in the `$distance_matrices`
#' field.
#' @param choices A dataframe indicating whether a preprocessing step was
#' applied or not, for each preprocessing step. This is generated by the
#' `factorial_preprocessing()` function and returned in the `$choices` field.
#' @param labels Optional argument giving names for each preprocessing step.
#' This is generated by the `factorial_preprocessing()` function and returned in
#' the `$labels` field.
#' @param baseline_index The index of the baseline distance matrix against which
#' we are comparing. Defaults to 128, which is the most minimal preprocessing
#' for our current implementation.
#' @param text_size The `cex` for text in dot plot generated by function.
#' @param num_comparisons The number of ranks to use
#' in calculating average difference. Defaults to 50.
#' @param parallel Logical indicating whether factorial prerpocessing should be
#' performed in parallel. Defaults to FALSE.
#' @param cores Defaults to 1, can be set to any number less than or equal to
#' the number of cores on one's computer.
#' @param verbose Logical indicating whether more information should be printed
#' to the screen to let the user know about progress. Defaults to TRUE.
#' @return A result list object.
#' @examples
#' \dontrun{
#' # *** This function is used automatically inside of the preText() function.
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
#' # run preText test
#' preText_test_results <-preText_test(scaling_results$distance_matrices,
#'                                     choices = preprocessed_documents$choices,
#'                                     labels = preprocessed_documents$labels,
#'                                     baseline_index = 128,
#'                                     text_size = 1,
#'                                     num_comparisons = 50,
#'                                     parallel = FALSE,
#'                                     cores = 1,
#'                                     verbose = TRUE)
#' }
#' @export
preText_test <- function(distance_matrices,
                       choices,
                       labels = NULL,
                       baseline_index = 128,
                       text_size = 1,
                       num_comparisons = 50,
                       parallel = FALSE,
                       cores = 1,
                       verbose = TRUE){

    method <- "distribution"
    num_dms <- length(distance_matrices)

    # figures out which entry in the distance matrix has the largest difference
    # from it to the base case entry.
    get_max_difference <- function(dist_matrix, baseline) {
      temp <- abs(dist_matrix - baseline)
      temp <- temp[lower.tri(temp)]
      ordering <- order(temp, decreasing = TRUE)
      return(ordering)
    }

    # find out the average rank of this distance difference in all of the other
    # preprocessing steps. Returns the average (absolute) rank difference
    compare_top_difference <- function(distance_matrices, baseline, 
                                       i, baseline_index, most_different_index, place,  ordering_list) {
      inds <- 1:length(distance_matrices)
      inds <- inds[-c(baseline_index, i)] #remove baselin and current dist_m
      rank_of_top_difference <- rep(0, length(inds))
      counter <- 1
      for (j in inds) {
        ## dist_mat <- distance_matrices[[j]]
        comparison_ordering <- ordering_list[[j]]
        rank_val <- which(comparison_ordering == most_different_index)
        rank_val <- abs(rank_val - place)
        rank_of_top_difference[counter] <- rank_val
        counter <- counter + 1
      }
      rank_of_top_difference <- rank_of_top_difference/length(lower.tri(distance_matrices[[j]])) #normalise rank of diffenrence by total number of diff
      average_rank <- mean(rank_of_top_difference) # mean diff
      return(list(ave_diff = average_rank, ave_diff_SE = sd(rank_of_top_difference)))
    }


    baseline <- distance_matrices[[baseline_index]]


    # make usre that we are not doing more comparisons than there are slots:
    new <- nrow(baseline)*(nrow(baseline) - 1)
    if (num_comparisons > new) {
        cat("Reducing number of comparisons to",new, "from",num_comparisons,
            "due to a lack of data for the requested number of comparisons.\n")
        num_comparisons <- new
    }
    scores <- rep(0,(num_dms - 1))
    score_SE <- rep(0,(num_dms - 1))
    score_counter <- 1
    ordering_list  <- list()

    if (method == "top") {
        # populate the scores vector
        for(i in 1:num_dms) {
            cat("Currently working on DFM:",i,"of",num_dms,"\n")
            # if we are not dealing with the baseline
            if (i != baseline_index) {
                dist_matrix <- distance_matrices[[i]]
                ordering <- get_max_difference(dist_matrix,
                                               baseline)
                most_different_index <- ordering[1]
                result <- compare_top_difference(distance_matrices,
                                                   baseline,
                                                   i,
                                                   baseline_index,
                                                   most_different_index,
                                                   place = 1)
                scores[score_counter] <- result$ave_diff
                score_SE[score_counter] <- result$ave_diff_SE
                score_counter <- score_counter + 1
            }

        }
    } else if (method == "distribution") {
        if (parallel) {
            dfms_to_use <- 1:num_dms
            cat("Preprocessing documents",length(dfms_to_use),
                "different ways on",
                cores,"cores. This may take a while...\n")
            cl <- parallel::makeCluster(getOption("cl.cores", cores))

            dfm_list <- parallel::clusterApplyLB(
                cl = cl,
                x = dfms_to_use,
                fun = parallel_rank_test,
                distance_matrices = distance_matrices,
                baseline = baseline,
                baseline_index = baseline_index,
                num_comparisons = num_comparisons)
            # stop the cluster when we are done
            parallel::stopCluster(cl)

            cat("Paralellization complete!")
            scores <- unlist(dfm_list)
            scores <- scores[-which(scores == 0)]
            print(scores)
        } else {
            # populate the scores vector
   for (i in 1:num_dms) {
          if (i != baseline_index) {
            ordering_list[[i]] <- get_max_difference(distance_matrices[[i]], baseline)
          }}
        for (i in 1:num_dms	) {
          if (verbose) {
            cat("Currently working on DFM:", i, "of", num_dms, 
                "\n")
          }
          ptm <- proc.time()
          if (i != baseline_index) {
            dist_matrix <- distance_matrices[[i]]
            ordering <- ordering_list[[i]]
            samp <- 1:num_comparisons
            cur_scores <- rep(0, num_comparisons)
            cur_score_SE <- rep(0, num_comparisons)
            for (k in 1:num_comparisons) {
              most_different_index <- ordering[samp[k]]
              result <- compare_top_difference(distance_matrices, 
                                               baseline, i, baseline_index, most_different_index, 
                                               place = samp[k], ordering_list )
              cur_scores[k] <- result$ave_diff
              cur_score_SE[k] <- result$ave_diff_SE
            }
            if (verbose) {
              cat("\n")
            }
            scores[score_counter] <- mean(cur_scores)
            score_SE[score_counter] <- mean(cur_score_SE)
            score_counter <- score_counter + 1 }
          t2 <- proc.time() - ptm
          if (verbose) {
            cat("Complete in:", t2[[3]], "seconds...\n") 
                }
            }
        }
    } else if (method == "continuous") {
        stop(paste("method:", method,"is not implemented..."))
    } else {
        stop(paste("method:", method,"is not valid..."))
    }

    labels <- labels[-baseline_index]
    data <- data.frame(Coefficient = scores[order(scores,decreasing = TRUE)],
                       SE = score_SE[order(scores,decreasing = TRUE)],
                       Coefficient_Type = "preText Score",
                       Variable = labels[order(scores,decreasing = TRUE)],
                       stringsAsFactors = FALSE)

    data$Variable <- factor(data$Variable,
                            levels = data$Variable[1:nrow(data)])

    ch <- choices[-baseline_index,]
    choice_diff <- rep(0,ncol(ch))
    pos_sd <- rep(0,ncol(ch))
    neg_sd <- rep(0,ncol(ch))
    for (i in 1:ncol(ch)) {
        pos <- scores[which(ch[,i])]
        neg <- scores[which(!ch[,i])]
        choice_diff[i] <- mean(pos) - mean(neg)
        pos_sd[i] <- sd(pos)
        neg_sd[i] <- sd(neg)
    }

    summary_data <- data.frame(decision = colnames(ch),
                               average_effect = choice_diff,
                               positive_sd = pos_sd,
                               negative_sd = neg_sd,
                               stringsAsFactors = FALSE)

    ret <- data.frame(preText_score = scores[order(scores,decreasing = TRUE)],
                      preprocessing_steps = labels[order(scores,decreasing = TRUE)],
                      stringsAsFactors = FALSE)

    ret2 <- data.frame(preText_score = scores,
                      preprocessing_steps = labels,
                      stringsAsFactors = FALSE)

    return(list(dfm_level_results = ret,
                dfm_level_results_unordered = ret2,
                summary_data = summary_data))
}
# for testing
# load("~/Dropbox/Preprocessing_Decisions/Data/Scaling/UK_Manifestos_Scaling_Results.Rdata")
# distance_matrices <- scaling_results$distance_matrices
# load("~/Dropbox/Preprocessing_Decisions/Data/128_Combination_Preprocessing_Labels.Rdata")
# load("~/Dropbox/Preprocessing_Decisions/Data/Scaling/Preprocessing_Choices.Rdata")
