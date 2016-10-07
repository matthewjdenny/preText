#' @title Plot Prevalence of Topic Key Terms
#' @description Plotting of key terms across preprocessing decisions.
#'
#' @param topic_key_term_results A data.frame with one column per key term and
#' one row for each set of topic model results. The entries in each cell should
#' be the proportion of topics in which a term appears.
#' @param labs Labels for the preprocessing specifications associated with each
#' set of topic model results.
#' @param key_term_columns The columns containing key term results.
#' @param custom_col_names Names for the key terms.
#' @param custom_labels Labels for the provided key. Must be of length 12.
#' @param one_matrix Logical indicating whether results should be displayed as
#' a one column matrix. Defaults to FALSE.
#' @param thresholds A numeric vector of length 11 with threshold for inclusion
#' in various heat map categories.
#' @param heat_ramp Option to use heat ramp (yellow-red-purple) instead of a
#' white to blue ramp.
#' @param return_data Logical indicating whether rescaled data should be
#' returned. Defaults to FALSE.
#' @return A plot
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
#' # !!!!!! You will need to look for some key terms, and store them in a
#' # data.frame. Your code should be based off of the following. !!!!
#' # function to search for a term
#' find_term <- function(vec, term) {
#'      tc <- 0
#'      for(i in 1:length(term)) {
#'          tc <- tc + sum(grepl(term[i],vec, ignore.case = T))
#'      }
#'      if (tc > 0) {
#'          return(TRUE)
#'      } else {
#'          return(FALSE)
#'      }
#' }
#'
#' # look for topics containing the terms below -- this is from our example with
#' # press releases so it will have to be modified.
#' # allows for multiple top terms related to the same concept
#' num_topics <- rep(0, length = 128)
#' search_list <- list(iraq = c("iraq"),
#'                     terror = c("terror"),
#'                     al_qaeda = c("qaeda"),
#'                     insurance = c("insur"),
#'                     stem_cell = c("stem"))
#'
#' # where we will store our results
#' topics_in_results <- data.frame(
#'     preprocessing_steps = preprocessed_documents$labels,
#'     iraq = num_topics,
#'     terror = num_topics,
#'     al_qaeda = num_topics,
#'     insurance = num_topics,
#'     stem_cell = num_topics,
#'     optimal_number_of_topics = optimal_k,
#'     stringsAsFactors = FALSE)
#' # count the number of topics in which each top term appears
#' for (i in 1:128) {
#'     # allows for multiple top terms related to the same concept
#'     top_terms <- top_terms_list[[i]]
#'     for (j in 1:length(search_list)) {
#'         in_topic <- apply(top_terms,2,find_term, term = search_list[[j]])
#'         which_topics <- which(in_topic)
#'         topics_in_results[i,(j+1)] <- length(which_topics)
#'     }
#' }
#' # now make a plot:
#' topic_key_term_plot(
#'      topics_in_results,
#'      preprocessed_documents$labels,
#'      key_term_columns  = 2:6,
#'      custom_col_names = c("Iraq", "Terrorism", "Al Qaeda", "Insurance", "Stem Cell"),
#'      custom_labels = c("0%","<1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%",
#'                        "8-9%","9-10%","10%+"),
#'      one_matrix = FALSE,
#'      thresholds = c(-0.0001,0,0.0099,0.0199,0.0299,0.0399,0.0499,0.0599,0.0699,
#'                     0.0799,0.0899,0.0999),
#'      heat_ramp = FALSE,
#'      return_data = FALSE)
#' }
#' @export
topic_key_term_plot <- function(
    topic_key_term_results,
    labs,
    key_term_columns  = 2:6,
    custom_col_names = c("Iraq", "Terrorism", "Al Qaeda", "Insurance", "Stem Cell"),
    custom_labels = c("0%","<1%","1-2%","2-3%","3-4%","4-5%","5-6%","6-7%","7-8%",
                      "8-9%","9-10%","10%+"),
    one_matrix = FALSE,
    thresholds = c(-0.0001,0,0.0099,0.0199,0.0299,0.0399,0.0499,0.0599,0.0699,
                   0.0799,0.0899,0.0999),
    heat_ramp = FALSE,
    return_data = FALSE) {

    variable <- value <- NULL

    custom_ramp <- c("#FFFFFF","#CDCEFA","#BEBDF6","#A7ABED","#959DE6","#7281D4",
                     "#5B70C9","#4A63C1","#3153B4","#1848A4","#053C96","#003A92")
    if (heat_ramp) {
        custom_ramp <- c("#4E4E4E","#F6F28F","#FEFA41","#FFDE03","#FFAF02","#FB4D00",
                         "#FE0524","#F306B9","#D705F0","#7018D1","#471BC1","#081CAF")
    }

    num_dfm <- nrow(topic_key_term_results)
    for (i in 1:num_dfm) {
        k <- topic_key_term_results$optimal_number_of_topics[i]
        raw <- topic_key_term_results[i,key_term_columns]
        topic_key_term_results[i,key_term_columns] <- raw/k
    }
    tktr <- topic_key_term_results[,key_term_columns]
    max_prop <- apply(tktr,2, max)

    to_return <- tktr

    # categorize the data
    uniq <- unique(c(as.matrix(tktr[,1:5])))
    uniq <- uniq[order(uniq)]

    # thresholds <- seq(from = 0,
    #                   to = max(max_prop),
    #                   length.out = length(custom_ramp) - 1) - 0.0001
    #
    # thresholds <- c(thresholds[1],0,thresholds[2:11])

    #now threshold the data and put it into the bins above
    find_color <- function(val, ramp) {
        length(which(ramp < val))
    }
    for (i in 1:num_dfm) {
        for (j in 1:ncol(tktr)) {
            tktr[i,j] <- find_color(tktr[i,j], thresholds)
        }
    }

    # put in custom colnames
    if (!is.null(custom_col_names)) {
        colnames(tktr) <- custom_col_names
    }

    tktr <- cbind(tktr, labs)
    to_return <- cbind(to_return, labs)
    data <- tktr

    data1 <- data[1:(nrow(data)/2),]
    data2 <- data[(nrow(data)/2 + 1):nrow(data),]

    data <- reshape2::melt(data)
    data1 <- reshape2::melt(data1)
    data2 <- reshape2::melt(data2)

    data$value <- as.factor(data$value)
    data1$value <- as.factor(data1$value)
    data2$value <- as.factor(data2$value)


    data$labs <- factor(data$labs, levels = rev(data$labs))
    data1$labs <- factor(data1$labs, levels = rev(data1$labs))
    data2$labs <- factor(data2$labs, levels = rev(data2$labs))

    # legend.position = "none",
    #make the plot
    if (!one_matrix) {
        p <- ggplot2::ggplot(data1, ggplot2::aes(variable, labs)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
            ggplot2::theme(plot.margin = ggplot2::unit(c(1,-.2,0.1,3.3),"cm"),
                           legend.position = "none") +
            ggplot2::ylab("") + ggplot2::xlab("")

        p <- p + ggplot2::scale_fill_manual("",
                                            values = custom_ramp,
                                            labels = custom_labels)

        p2 <- ggplot2::ggplot(data2, ggplot2::aes(variable, labs)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
            ggplot2::theme(plot.margin = ggplot2::unit(c(1,1,0.1,0),"cm")) +
            ggplot2::ylab("") + ggplot2::xlab("")

        p2 <- p2 + ggplot2::scale_fill_manual("",
                                              values = custom_ramp,
                                              labels = custom_labels)

        p <- cowplot::ggdraw(cowplot::switch_axis_position(p , axis = 'x'))
        p2 <- cowplot::ggdraw(cowplot::switch_axis_position(p2 , axis = 'x'))

        multiplot(p, p2, cols = 2)
    } else {
        p <- ggplot2::ggplot(data, ggplot2::aes(variable, labs)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "black") +
            ggplot2::theme(plot.margin = ggplot2::unit(c(1,1,0.1,0),"cm")) +
            ggplot2::ylab("") + ggplot2::xlab("")

        p <- p + ggplot2::scale_fill_manual("",
                                            values = custom_ramp,
                                            labels = custom_labels)

        p <- cowplot::ggdraw(cowplot::switch_axis_position(p , axis = 'x'))
        print(p)
    }

    if (return_data) {
        return(to_return)
    }
}
