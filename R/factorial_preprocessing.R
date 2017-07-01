#' @title Create factorial combinations of pre-processing choices.
#' @description Checks user input or supplies default values
#'
#' @param choices an ordered and named list of logical vectors which indicator
#' which pre-processing parameters to include in the factorial combination of
#' choices. The list must include the following entries (in order):
#' removePunctuation, removeNumbers, lowercase, stem, removeStopwords,
#' infrequent_terms, use_ngrams. 
factorial_combinations <- function(choices) {
    # sanity check for user-supplied list of choices
    if (class(choices)[1] != 'list') {
        stop('choices argument must be a list')
    }
    args <- c("removePunctuation", "removeNumbers", "lowercase", "stem",
              "removeStopwords", "infrequent_terms", "use_ngrams")
    if (!identical(names(choices), args)) {
        stop('choices must be an ordered list of logical vectors with names: ', paste(args, collapse = ', '))
    }
    if (any(sapply(choices, class) != 'logical')) {
        stop('all elements of the choices list must be logical')
    }

    # create choices data frame
    choices <- expand.grid(choices)

    # create labels
    labels <- c("P","N","L","S","W","I","3")
    labels <- apply(as.matrix(choices), 1, function(x) paste(labels[x], collapse = '-'))

    # output
    out <- list('choices' = choices, 'labels' = labels)
    return(out)
}

#' @title A function to perform factorial preprocessing of a corpus of texts
#' into quanteda document-frequency matrices.
#' @description Preprocesses a corpus of texts into a document-frequency matrix
#' in 128 different ways.
#'
#' @param text A vector of strings (one per document) or quanteda corpus object
#' from which we wish to form a document-term matrix.
#' @param use_ngrams Option to extract 1,2, and 3-grams from the text as another
#' potential preprocessing step. Defaults to TRUE.
#' @param infrequent_term_threshold A proportion threshold at which infrequent
#' terms are to be filtered. Defaults to 0.01 (terms that appear in less than
#' 1 percent of documents).
#' @param cores Defaults to 1. When cores > 1, preText will use parallel
#' computation. can be set to any number less than or equal to the number of
#' cores on one's computer.
#' @param intermediate_directory Optional path to a directory where each dfm
#' will be saved as an intermediate step. The file names will follow the
#' convention intermediate_dfm_i.Rdata, where i is the index of the combination
#' of preprocessing choices. The function will then attempt to read all of the
#' dfm's back into a list if return_results = TRUE (by default), or simply end
#' the function call if return_results = FALSE. This can be a useful option if
#' the user is preprocessing a corpus that would make a dfm list that was
#' impractical to work with due to its size.
#' @param parameterization_range Defaults to NULL, but can be set to a numeric
#' vector of indexes relating to preprocessing decisions. This can be used to
#' restart large analyses after power failure.
#' @param return_results Defaults to TRUE, can be set to FALSE to prevent an
#' overly large dfm list from being created.
#' @param choices an ordered and named list of logical vectors which indicator
#' which pre-processing parameters to include in the factorial combination of
#' choices. Leave NULL to use preText's default combinations. User-submitted
#' lists must include the following entries (in order): removePunctuation,
#' removeNumbers, lowercase, stem, removeStopwords, infrequent_terms,
#' use_ngrams. 
#' @param verbose Logical indicating whether more information should be printed
#' to the screen to let the user know about progress in preprocessing. Defaults
#' to TRUE.
#' @return A list object containing permutations of the document-term matrix.
#' @examples
#' \dontrun{
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
#' }
#' @export
factorial_preprocessing <- function(text,
                                    use_ngrams = TRUE,
                                    infrequent_term_threshold = 0.01,
                                    cores = 1,
                                    intermediate_directory = NULL,
                                    parameterization_range = NULL,
                                    return_results = TRUE,
                                    choices = NULL,
                                    verbose = TRUE){

    # set some intermediate variables
    cur_directory <- getwd()

    # set working directory if given
    if (!is.null(intermediate_directory)) {
        setwd(intermediate_directory)
    } else {
        intermediate_directory <- cur_directory
    }

    # check to see if input is a corpus object. If it is, extract the texts
    if (!quanteda::is.corpus(text)) {
        text <- quanteda::corpus(text)
    }

    # now make sure we have a character vector
    if (!quanteda::is.corpus(text)) {
        stop("You must provide either a character vector of strings (one per document, or a quanteda corpus object.")
    }

    # factorial combinations of pre-processing choices
    if (is.null(choices)) {
        choices <- list(removePunctuation = c(TRUE, FALSE),
                        removeNumbers = c(TRUE, FALSE),
                        lowercase = c(TRUE, FALSE),
                        stem = c(TRUE, FALSE),
                        removeStopwords = c(TRUE, FALSE),
                        infrequent_terms = c(TRUE, FALSE),
                        use_ngrams = c(TRUE, FALSE))
    }
    if (!use_ngrams) {
        choices$use_ngrams = FALSE
    }
    labels <- factorial_combinations(choices)$labels
    choices <- factorial_combinations(choices)$choices
    cat("Preprocessing", length(text$documents$texts), "documents ", nrow(choices), " different ways...\n")

    # create a list object in which to store the different dfm's
    dfm_list <- vector(mode = "list", length = nrow(choices))

    # row range to work on.
    rows_to_preprocess <- 1:nrow(choices)
    if(!is.null(parameterization_range)){
        rows_to_preprocess <- parameterization_range
    }

    if (cores > 1) {
        cat("Preprocessing documents",
            length(rows_to_preprocess),
            "different ways on",
            cores,
            "cores. This may take a while...\n")

        # execute in parallel
        if (.Platform$OS.type == 'unix') { # linux & mac
            dfm_list <- parallel::mclapply(rows_to_preprocess,
                                           parallel_preprocess,
                                           choices = choices,
                                           text = text,
                                           infrequent_term_threshold = infrequent_term_threshold,
                                           intermediate_directory = intermediate_directory,
                                           verbose = verbose,
                                           save_intermediate = TRUE,
                                           mc.cores = cores,
                                           mc.preschedule = FALSE)
        } else { # windows
            cl <- parallel::makeCluster(getOption("cl.cores", cores)) 
            dfm_list <- parallel::clusterApplyLB(cl = cl,
                                                 x = rows_to_preprocess,
                                                 parallel_preprocess,
                                                 choices = choices,
                                                 text = text,
                                                 infrequent_term_threshold = infrequent_term_threshold,
                                                 intermediate_directory = intermediate_directory,
                                                 verbose = verbose,
                                                 save_intermediate = TRUE)
            parallel::stopCluster(cl) # stop the cluster when we are done
        }
    } else {
        # loop over different preprocessing decisions
        for (i in rows_to_preprocess) {
            current_dfm <- parallel_preprocess(i, 
                                               choices = choices, 
                                               text = text,
                                               infrequent_term_threshold = infrequent_term_threshold,
                                               intermediate_directory = intermediate_directory,
                                               verbose = verbose,
                                               save_intermediate = FALSE) 
            dfm_list[[i]] <- current_dfm
        }
    }

    # if we are returning results and using parallel, then read in the
    # intermediate dfm's
    if (return_results & (cores > 1)) {
        cat("Preprocessing complete, loading in intermediate DFMs...\n")
        dfm_list <- vector(mode = "list", length = nrow(choices))
        for (i in 1:length(dfm_list)) {
            load(paste("intermediate_dfm_",i,".Rdata",sep = ""))
            dfm_list[[i]] <- current_dfm
        }
    }

    names(dfm_list) <- labels
    rownames(choices) <- labels

    # combine metadata and dfm list and return
    return_list <- list(choices = choices,
                        dfm_list = dfm_list,
                        labels = labels)

    # reset the directory
    setwd(cur_directory)

    # return results
    return(return_list)
}
