#' @title Plot Prevalence of Topiuc Key Terms
#' @description Plotting of key terms across preprocessing decisions.
#'
#' @param topic_key_term_results A data.frame.
#' @param labs Labels for the preprocessing choices being used in the analysis.
#' @param key_term_columns The columns containing key term results.
#' @param custom_col_names Names for the key terms.
#' @param custom_labels Labels for the provided key. Must be of length 12.
#' @param one_matrix Logical indicating whether results shoudl be displayed as
#' a one column matrix. Defaults to FALSE.
#' @param thresholds A numeric vector of length 11 with threshold for inclusion
#' in various heat map categroies.
#' @param heat_ramp Option to use heat ramp (yellow-red-purple) instead of a
#' white tp blue ramp.
#' @param return_data Logical indicating whether recaled data should be
#' returned. Defaults to FALSE.
#' @return A plot
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

    # gray - yellow - orange - red - purple
    # custom_ramp <- c("#4E4E4E",
    #                  "#F6F28F",
    #                  "#FAF571",
    #                  "#FEFA41",
    #                  "#FFDE03",
    #                  "#FFAF02",
    #                  "#FB4D00",
    #                  "#FE0524",
    #                  "#F5067B",
    #                  "#F306B9",
    #                  "#D705F0",
    #                  "#9C10E3",
    #                  "#7018D1",
    #                  "#471BC1",
    #                  "#081CAF")
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
