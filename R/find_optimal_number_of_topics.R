find_optimal_number_of_topics <- function(
   i,
   dfm_list,
   topics,
   intermediate_file_directory,
   intermediate_file_names,
   cv_train_di,
   cv_test_di) {

    current_dfm <- NULL
    if (is.null(dfm_list)) {
        # if no dfm_list is provided then load in from intermediate file
        setwd(intermediate_file_directory)
        load(intermediate_file_names[i])
        cur_dfm <- current_dfm
    } else {
        cur_dfm <- dfm_list[[i]]
    }

    # create matrix to hold perplexities for each cv split and number of topics
    cv_perplexities <- matrix(0,
                              nrow = length(cv_train_di),
                              ncol = length(topics))

    # loop over cross validation splits
    for (k in 1:length(cv_train_di)) {
        cat("working on cv split",k,"of",length(cv_train_di),"\n" )
        # get the current test and train matrices
        train_dfm <- cur_dfm[cv_train_di[[k]],]
        test_dfm <- cur_dfm[cv_test_di[[k]],]

        cv_perplexities[k,] <- get_perplexities(topics,
                                                train_dfm,
                                                test_dfm)
        cat("The current perplexities are:\n")
        print(cv_perplexities[k,])
        cat("\n")
    }

    # get column means
    average_perplexities <- apply(cv_perplexities,2,mean)

    optimal_k <- topics[which(average_perplexities == min(average_perplexities))[1]]

    cat("The optimal number of topics for dfm",i,"is:",optimal_k,"\n")

    # find the (constrained) optimum using the optimize function -- we can't
    # practically do this if we are using cross validation.
    # optimum_k <- stats::optimize(f = get_perplexities,
    #                              cur_dfm = cur_dfm,
    #                              interval = interval,
    #                              maximum = FALSE)

    perplexities <- list(
        cv_perplexities = cv_perplexities,
        average_perplexities = average_perplexities,
        optimal_k = optimal_k)

    # return or save, depending on whether we are working with intermediate
    # files.
    if (is.null(dfm_list)) {
        # if no dfm_list is provided then load in from intermediate file
        save(perplexities, file = paste("perplexities_",i,".RData",sep = ""))
        return(i)
    } else {
        return(list(results = perplexities))
    }


}
