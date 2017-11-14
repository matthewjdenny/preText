#' @title Plot of Wordfish rankings of documents
#' @description Coloration by ground-truth ranking.
#'
#' @param wordfish_results The output from the `wordfish_comparison()` function.
#' @param labels A character vector giving the names for each preprocessing step.
#' @param invert Logical indicating whether Wordfish score rankings should be
#' reversed internally.
#' @param ranking A character vector containing the correctly ranked document
#' names.
#' @param black_white Logical, defaults to FALSE. If FALSE then results are
#' displayed on a red-blue scale. IF TRUE, then mis-ordered documents are
#' colored black.
#' @param one_matrix Logical indicating whether results should be plotted as a
#' one or two column matrix. Defaults to FALSE.
#' @param return_deviations Return a dataset indicating the ordering deviations
#' for each preprocessing combination. Defaults to FALSE.
#' @return A plot.
#' @examples
#' \dontrun{
#' # replicates Wordfish analysis from Denny and Spirling (2016)
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
#' # get the years each document was written and store them as a numeric vector
#' dfm <- preprocessed_documents$dfm_list[[1]]
#' rl <- function(str) {
#'     stringr::str_replace_all(str,"[A-Za-z]+","")
#' }
#' years <- as.numeric(sapply(rownames(dfm),rl))
#'
#' # use the wordfish_comparison function to compare all dfms. We are using
#' # conservative and labour manifestos from 1983, 1987, 1992, and 1997 for a total
#' # of 8 manifestos. These are indicated by the document_inidices = c(19:22,42:45)
#' # argument. You can see the document names by entering rownames(dfm) into the
#' # console. We need to set the anchors to 5,1 because anchoring is applied in the
#' # reduced dfm. We are also only including terms that appear atleast once in a
#' # manifesto from each of the 4 years, to deal with the strong temporal effects.
#' wordfish_results <- wordfish_comparison(
#'     preprocessed_documents$dfm_list,
#'     years,
#'     anchors = c(1,5),
#'     proportion_threshold = 1,
#'     document_inidices = c(19:22,42:45))
#' deviations <- wordfish_rank_plot(wordfish_results,
#'                   labels = preprocessed_documents$labels,
#'                   invert = FALSE,
#'                   ranking = c("Lab1983","Lab1987","Lab1992","Lab1997",
#'                               "Con1997","Con1992","Con1987","Con1983"),
#'                   black_white = FALSE,
#'                   one_matrix = FALSE,
#'                   return_deviations = FALSE)
#' }
#' @export
wordfish_rank_plot <- function(
    wordfish_results,
    labels,
    invert = TRUE,
    ranking = c("Lab1983","Lab1987","Lab1992","Lab1997",
                "Con1997","Con1992","Con1987","Con1983"),
    black_white = FALSE,
    one_matrix = FALSE,
    return_deviations = FALSE) {

    variable <- value <- NULL

    orders <- wordfish_results$results_by_dfm

    num_dfm <- length(orders)
    data <- matrix(0,nrow = num_dfm, ncol = length(ranking))
    data3 <- matrix(0,nrow = num_dfm, ncol = length(ranking))
    for (i in 1:num_dfm) {
        cur <- orders[[i]]$document
        if (invert) {
            cur <- rev(cur)
        }
        for(j in 1:length(ranking)) {
            data[i,j] <- which(ranking == cur[j])
        }
        # if we are doing the black and white scheme, the binarize based on
        # correct position
        if (black_white) {
            for(j in 1:length(ranking)) {
                if (data[i,j] == j) {
                    data3[i,j] <- 0
                } else {
                    data3[i,j] <- 1
                }
            }
        }
    }

    # calcualte difference from correct ordering
    difference <- rep(0, num_dfm)
    right_ends <- rep(0, num_dfm)
    out_of_order <- rep(0, num_dfm)
    for (i in 1:num_dfm) {
        cd <- 0
        cd2 <- 0
        for (j in 1:ncol(data)) {
            # print(abs(j - data[i,j]))
            cd <- cd + abs(j - data[i,j])
            if(abs(j - data[i,j]) >0 ) {
                cd2 <- cd2 + 1
            }
        }
        if (data[i,1] != 1) {
            right_ends[i] <- right_ends[i] + 1
        }
        if (data[i,ncol(data)] != ncol(data)) {
            right_ends[i] <- right_ends[i] + 1
        }

        out_of_order[i] <- cd2
        difference[i] <- cd
    }

    data <- data - 4.5
    colnames(data) <- ranking
    rownames(data) <- labels
    data <- as.data.frame(data, stringsAsFactors = F)
    data <- cbind(labels,data, stringsAsFactors = F)

    data <- data[order(difference, decreasing = F),]
    if (black_white) {
        data3 <- data3 - .5
        colnames(data3) <- ranking
        rownames(data3) <- labels
        data3 <- as.data.frame(data3, stringsAsFactors = F)
        data3 <- cbind(labels,data3, stringsAsFactors = F)
        data <- data3[order(difference, decreasing = F),]
    }

    deviation_data <- data.frame(preprocessing_combination = labels,
                                 rank_deviation = difference,
                                 num_out_of_order = out_of_order,
                                 wrong_ends = right_ends,
                                 stringsAsFactors = FALSE)

    data1 <- data[1:(nrow(data)/2),]
    data2 <- data[(nrow(data)/2 + 1):nrow(data),]
    data4 <- data

    data1 <- reshape2::melt(data1)
    data2 <- reshape2::melt(data2)
    data4 <- reshape2::melt(data4)

    # we need to do this manual reordering of the labels so that we can show
    # things in order from most to least similar to the a-priori ordering.
    data4$labels <- factor(data4$labels, levels = rev(data$labels))

    #make the plot
    UMASS_BLUE <- rgb(51,51,153,255,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,255,maxColorValue = 255)

    if (!one_matrix) {
        p <- ggplot2::ggplot(data1, ggplot2::aes(variable, labels)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "gray50") +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_text(angle=45, vjust = 1.2,
                                                               hjust = 0),
                           plot.margin = ggplot2::unit(c(1,.6,0.1,0),"cm")) +
            ggplot2::ylab("") + ggplot2::xlab("") +
            ggplot2::scale_x_discrete(position = "top")
        if (black_white) {
            p <- p + ggplot2::scale_fill_gradient2(low = "white", high = "black")
        } else {
            p <- p + ggplot2::scale_fill_gradient2(low = UMASS_RED, high = UMASS_BLUE)
        }
        p2 <- ggplot2::ggplot(data2, ggplot2::aes(variable, labels)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "gray50") +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_text(angle=45, vjust =1.2,
                                                               hjust = 0),
                           plot.margin = ggplot2::unit(c(1,1.2,0.1,-.6),"cm")) +
            ggplot2::ylab("") + ggplot2::xlab("") +
            ggplot2::scale_x_discrete(position = "top")
        if (black_white) {
            p2 <- p2 + ggplot2::scale_fill_gradient2(low = "white", high = "black")
        } else {
            p2 <- p2+ ggplot2::scale_fill_gradient2(low = UMASS_RED, high = UMASS_BLUE)
        }

        p <- cowplot::ggdraw(p)
        p2 <- cowplot::ggdraw(p2)

        multiplot(p, p2, cols = 2)
    } else {
        p <- ggplot2::ggplot(data4, ggplot2::aes(variable, labels)) +
            ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "gray50") +
            ggplot2::theme(legend.position = "none",
                           axis.text.x = ggplot2::element_text(angle=45, vjust = 1.2,
                                                               hjust = 0),
                           axis.text.y = ggplot2::element_blank(),
                           axis.ticks.y = ggplot2::element_blank(),
                           plot.margin = ggplot2::unit(c(1,1,0.1,0),"cm")) +
            ggplot2::ylab("") + ggplot2::xlab("") +
            ggplot2::scale_x_discrete(position = "top")
        if (black_white) {
            p <- p + ggplot2::scale_fill_gradient2(low = "white", high = "black")
        } else {
            p <- p + ggplot2::scale_fill_gradient2(low = UMASS_RED, high = UMASS_BLUE)
        }
        p <- cowplot::ggdraw(p)
        print(p)
    }

    if (return_deviations) {
        cat("Returing results...\n")
        return(deviation_data)
    }
}
