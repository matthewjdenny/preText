regression_coefficient_plot <- function(data,
                                        text_size = 1,
                                        remove_intercept = FALSE) {
    #define colors
    Model <- Variable <- Coefficient <- SE  <- NULL

    # if we are only using the one model, proceed as normal.

    if (remove_intercept) {
        data <- data[-which(data$Variable == "Intercept"),]
    }

    zp1 <- ggplot2::ggplot(data, ggplot2::aes(colour = Model)) +
        ggplot2::scale_colour_brewer(palette = "Set1") +
        ggplot2::theme(axis.text = ggplot2::element_text(size = text_size))
    zp1 <- zp1 + ggplot2::geom_vline(xintercept = 0,
                                     colour = gray(1/2),
                                     lty = 2)
    zp1 <- zp1 + ggplot2::geom_segment(
        ggplot2::aes(y = Variable,
                     yend = Variable,
                     x = Coefficient - SE*(-qnorm((1 - 0.95)/2)),
                     xend = Coefficient + SE*(-qnorm((1 - 0.95)/2))),
        lwd = 1)
    zp1 <- zp1 + ggplot2::geom_point(
        ggplot2::aes(y = Variable,
                     x = Coefficient),
        lwd = 2,
        shape = 21, fill = "WHITE")

    zp1 <- zp1  + ggplot2::theme_bw() +
        facet_grid(~ Model, scales = "free_x") +
        ggplot2::theme(legend.position = "none",
                       axis.text.x=element_text(angle=-45, hjust=0)) +
        ggplot2::ylab("") +
        ggplot2::xlab("Regression Coefficient")
    #ggplot2::xlab("Normalized Effect on Rank Difference")

    print(zp1)
}