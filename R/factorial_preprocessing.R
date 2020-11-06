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
#' @param parallel Logical indicating whether factorial preprocessing should be
#' performed in parallel. Defaults to FALSE.
#' @param cores Defaults to 1, can be set to any number less than or equal to
#' the number of cores on one's computer.
#' @param save_dfm Logical indication if each preprocessed dataframe
#' should be saved as it's own Rdata object
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
                                    parallel = FALSE,
                                    cores = 1,
                                    save_dfm = FALSE,
                                    intermediate_directory = NULL,
                                    parameterization_range = NULL,
                                    return_results = TRUE,
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
    if (quanteda::is.corpus(text)) {
      text <- quanteda::texts(text)
    } else if (!is.character(text)) {
      stop("You must provide either a character vector of strings (one per document, or a quanteda corpus object.")
    }

    # create a data.frame with factorial combinations of all choices.
    if (use_ngrams) {
        cat("Preprocessing", length(text), "documents 128 different ways...\n")
        choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                               removeNumbers = c(TRUE,FALSE),
                                               lowercase = c(TRUE,FALSE),
                                               stem = c(TRUE,FALSE),
                                               removeStopwords = c(TRUE,FALSE),
                                               infrequent_terms = c(TRUE, FALSE),
                                               use_ngrams = c(TRUE, FALSE))))

        labels <- rep("",128)
        # indicators of different preprocessing steps
        labs <- c("P","N","L","S","W","I","3")
        for (i in 1:128) {
            str <- ""
            for (j in 1:7) {
                if (choices[i,j]) {
                    if (str == "") {
                        str <- labs[j]
                    } else {
                        str <- paste(str,labs[j],sep = "-")
                    }
                }
            }
            labels[i] <- str
        }
    } else {
        cat("Preprocessing", length(text), "documents 64 different ways...\n")
        choices <- data.frame(expand.grid(list(removePunctuation = c(TRUE,FALSE),
                                               removeNumbers = c(TRUE,FALSE),
                                               lowercase = c(TRUE,FALSE),
                                               stem = c(TRUE,FALSE),
                                               removeStopwords = c(TRUE,FALSE),
                                               infrequent_terms = c(TRUE, FALSE),
                                               use_ngrams = c(FALSE))))

        labels <- rep("",64)
        # indicators of different preprocessing steps
        labs <- c("P","N","L","S","W","I","3")
        for (i in 1:64) {
            str <- ""
            for (j in 1:7) {
                if (choices[i,j]) {
                    if (str == "") {
                        str <- labs[j]
                    } else {
                        str <- paste(str,labs[j],sep = "-")
                    }
                }
            }
            labels[i] <- str
        }
    }

    
    # create a list object in which to store the different dfm's
    dfm_list <- vector(mode = "list", length = nrow(choices))

    # row range to work on.
    
    if(!is.null(parameterization_range)){
        choices <- choices[parameterization_range, ] # If I understand correctly parameterization_range is to restart the process, but I'm not sure where the number come from.
    }

    rows_to_preprocess <- 1:nrow(choices)
    if (parallel) {
        cat("Preprocessing documents",nrow(rows_to_preprocess),
            "different ways on", cores,"cores. This may take a while...\n")
        
        cl <- parallel::makeCluster(getOption("cl.cores", cores)) #makes cluster

        dfm_list <- parallel::clusterApplyLB(cl,
                          x = split(choices, seq(nrow(choices))), # convert the choices df as list to iterate over
                          fun = preprocessing_pipeline,
                          text = text,
                          infrequent_term_threshold = infrequent_term_threshold ,
                          verbose = verbose ,
                          intermediate_directory = intermediate_directory) ## TODO tmp directory
        
        parallel::stopCluster(cl) # stop the cluster when we are done
  }  
    else { # loop over different preprocessing decisions
        for (i in rows_to_preprocess) {
            if (verbose) {
                cat("Currently working on combination",i,"of",nrow(choices),"\n")
            }
            current_dfm <- preprocessing_pipeline(text = text,
                                                  choices = choices[i,],
                                                  infrequent_term_threshold= infrequent_term_threshold,
                                                  verbose = verbose)           
            dfm_list[[i]] <- current_dfm  # store the current dfm
        }
    }

    # if we are returning results and using parallel, then read in the
    # intermediate dfm's
    if (return_results & save_dfm) {
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

