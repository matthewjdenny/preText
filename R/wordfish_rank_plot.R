#' @title Plot of Wrodfish rankings of documents
#' @description Coloration by ground-truth ranking.
#'
#' @param wordfish_results The output from the `wordfish_comparison()` function.
#' @param labels A character vector giving the names for each preprocessing step.
#' @param invert Logical indicating whether wordfish score rankings should be
#' reversed internally.
#' @param ranking A character vector containing the correctly ranked document
#' names.
#' @param black_white Logical, defualts to FALSE. If FALSE then results are
#' displayed on a red-blue scale. IF TRUE, then mis-ordered documents are
#' colored black.
#' @return A plot.
#' @export
wordfish_rank_plot <- function(
    wordfish_results,
    labels,
    invert = TRUE,
    ranking = c("Lab1983","Lab1987","Lab1992","Lab1997",
                "Con1997","Con1992","Con1987","Con1983"),
    black_white = FALSE) {

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
    for (i in 1:num_dfm) {
        cd <- 0
        for (j in 1:ncol(data)) {
            print(abs(j - data[i,j]))
            cd <- cd + abs(j - data[i,j])
        }
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


    data1 <- data[1:(nrow(data)/2),]
    data2 <- data[(nrow(data)/2 + 1):nrow(data),]

    data1 <- reshape2::melt(data1)
    data2 <- reshape2::melt(data2)

    data1$labels <- factor(data1$labels, levels = rev(data1$labels))
    data2$labels <- factor(data2$labels, levels = rev(data2$labels))

    #make the plot
    UMASS_BLUE <- rgb(51,51,153,255,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,255,maxColorValue = 255)

    p <- ggplot2::ggplot(data1, ggplot2::aes(variable, labels)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "white") +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle=45, vjust = 1.2,
                                                           hjust = 1.2),
                       plot.margin = unit(c(1,.6,0.1,0),"cm")) +
        ggplot2::ylab("") + ggplot2::xlab("")
    if (black_white) {
        p <- p + ggplot2::scale_fill_gradient2(low = "white", high = "black")
    } else {
        p <- p + ggplot2::scale_fill_gradient2(low = UMASS_RED, high = UMASS_BLUE)
    }
    p2 <- ggplot2::ggplot(data2, ggplot2::aes(variable, labels)) +
        ggplot2::geom_tile(ggplot2::aes(fill = value), colour = "white") +
        ggplot2::theme(legend.position = "none",
                       axis.text.x = ggplot2::element_text(angle=45, vjust =1.2,
                                                           hjust = 1.2),
                       plot.margin = unit(c(1,1.2,0.1,-.6),"cm")) +
        ggplot2::ylab("") + ggplot2::xlab("")
    if (black_white) {
        p2 <- p2 + ggplot2::scale_fill_gradient2(low = "white", high = "black")
    } else {
        p2 <- p2+ ggplot2::scale_fill_gradient2(low = UMASS_RED, high = UMASS_BLUE)
    }

    p <- cowplot::ggdraw(switch_axis_position(p , axis = 'x'))
    p2 <- cowplot::ggdraw(switch_axis_position(p2 , axis = 'x'))

    multiplot(p, p2, cols = 2)
}
