#' @title Optimal Topic Model k Comparison
#' @description Calculate the optimal number of topics for LDA using perplexity
#' for each dfm.
#'
#' @param cross_validation_train_document_indicies A list of numeric vectors
#' where the length of the list is equal to the number of splits to be used
#' for cross validation, and each vector contains the numeric indices of
#' documents to be used for training.
#' @param cross_validation_test_document_indicies A list of numeric vectors
#' where the length of the list is equal to the number of splits to be used
#' for cross validation, and each vector contains the numeric indices of
#' documents to be used for testing.
#' @param dfm_object_list An optional list of quanteda dfm() objects. If none
#' are provided, then intermediate files will be used.
#' @param topics A numeric vector containing the numbers of topics to search
#' over. Defaults to `c(2,5,10,20,30,40,50,60,70,80,90,100)`.
#' @param names optional names for each dfm to make downstream interpretation
#' easier. Defaults to NULL.
#' @param parallel Logical indicating whether model fitting should be
#' performed in parallel. Defaults to FALSE.
#' @param cores Defaults to 1, can be set to any number less than or equal to
#' the number of cores on one's computer.
#' @param intermediate_file_directory Optional directory containing Rdata files
#' for each of the factorial preprocessing combinations.
#' @param intermediate_file_names Optional vector of file names for intermediate
#' Rdata files -- one per combination.
#' @return A vector containing the optimal k for each dfm.
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
#' }
#' @export
optimal_k_comparison <- function(
    cross_validation_train_document_indicies,
    cross_validation_test_document_indicies,
    dfm_object_list = NULL,
    topics = c(2,5,10,20,30,40,50,60,70,80,90,100),
    names  = NULL,
    parallel = FALSE,
    cores = 1,
    intermediate_file_directory = NULL,
    intermediate_file_names = NULL){

    # get the number of dfms
    if (!is.null(dfm_object_list)) {
        num_dfms <- length(dfm_object_list)
    } else {
        num_dfms <- length(intermediate_file_names)
    }

    # ceate data structures to store information
    optimal_k <- vector(mode = "list",length = num_dfms)

    if (length(cross_validation_train_document_indicies) !=
        length(cross_validation_test_document_indicies)) {
        stop("You must provide lists of test and train document indices that are of the same length!")
    }

    if (parallel) {
        cat("Finding optimal K from LDA",num_dfms,"different ways on",
            cores,"cores. This may take a while...\n")
        cl <- parallel::makeCluster(getOption("cl.cores", cores))

        optimal_k <- parallel::clusterApplyLB(
            cl = cl,
            x = 1:num_dfms,
            fun = find_optimal_number_of_topics,
            dfm_list = dfm_object_list,
            topics = topics,
            intermediate_file_directory = intermediate_file_directory,
            intermediate_file_names = intermediate_file_names,
            cv_train_di = cross_validation_train_document_indicies,
            cv_test_di = cross_validation_test_document_indicies)
        # stop the cluster when we are done
        parallel::stopCluster(cl)
    } else {
        for (i in 1:num_dfms) {
            cat("Currently working on dfm",i,"of",num_dfms,"\n")
            ptm <- proc.time()
            # apply temporal filter

            optimal_k[[i]] <- find_optimal_number_of_topics(
                i = i,
                dfm_list = dfm_object_list,
                topics = topics,
                intermediate_file_directory = intermediate_file_directory,
                intermediate_file_names = intermediate_file_names,
                cv_train_di = cross_validation_train_document_indicies,
                cv_test_di = cross_validation_test_document_indicies)

            t2 <- proc.time() - ptm
            cat("Complete in:",t2[[3]],"seconds...\n")
        }
    }

    if (!is.null(names)) {
        names(optimal_k) <- names
    }

    return(optimal_k)
}



