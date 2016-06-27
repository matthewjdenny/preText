#' @title Document Position Plots
#' @description Plot rotated scaled positions of documents under each
#' preprocessing regime.
#'
#' @param positions_list A list of scaled document positions.
#' @param num_cols The number of columns to use in combining plots.
#' @param colors Optional vector of document colors to distinguish groups.
#' @param decision_colors Defaults to NULL, if desired, the user
#' should provide a vector of logical values of length equal to the number of
#' preprocessing decisions. Can be used to bifurcate the points within a
#' single plot to show the effects of a particular decision. Points in the
#' TRUE class will be colors BLUE and those in the FALSE class will be colored
#' red.
#' @return list of ggplot2 objects until I get things fixed.
#' @export
document_position_plots <- function(positions_list,
                                    num_cols = 10,
                                    colors = NULL,
                                    decision_colors = NULL){

    UMASS_BLUE <- rgb(51,51,153,195,maxColorValue = 255)
    UMASS_RED <- rgb(153,0,51,195,maxColorValue = 255)
    UMASS_GREEN <- rgb(0,102,102,195,maxColorValue = 255)
    UMASS_YELLOW <- rgb(255,255,102,255,maxColorValue = 255)
    UMASS_ORANGE <- rgb(255,204,51,195,maxColorValue = 255)
    UMASS_PURPLE <- rgb(65,39,59,195,maxColorValue = 255)
    UMASS_BROWN <- rgb(148,121,93,195,maxColorValue = 255)

    # get the number of dfms
    num_dfms <- length(positions_list)
    # get anchor scaled psoitions
    anchor_positions <- positions_list[[1]]
    anchor_positions <- anchor_positions[order(rownames(anchor_positions)),]
    document_names <- rownames(anchor_positions)

    positions <- data.frame(Dim_1 = anchor_positions[,1],
                            Dim_2 = anchor_positions[,2],
                            Document = row.names(anchor_positions),
                            stringsAsFactors = FALSE)
    # rotate the other positions and put them together in a big data.frame
    for (i in 2:num_dfms) {
        cur_pos <- positions_list[[i]][order(rownames(positions_list[[i]])),]
        cur <- vegan::procrustes(anchor_positions,
                                 cur_pos,
                                 scale = F)$Yrot

        cur_df <- data.frame(Dim_1 = cur[,1],
                             Dim_2 = cur[,2],
                             Document = row.names(cur),
                             stringsAsFactors = FALSE)
        positions <- rbind(positions, cur_df)
    }

    if (is.null(decision_colors)) {
        if (is.null(colors)) {
            colors <- rep(UMASS_BLUE,nrow(anchor_positions))
        }
    } else {
        colors <- rep("", length(decision_colors))
        for(i in 1:length(decision_colors)) {
            if (decision_colors[i]) {
                colors[i] <- UMASS_BLUE
            } else {
                colors[i] <- UMASS_RED
            }
        }
    }


    # loop over unique row observations
    ggplot_list <- vector(mode = "list", length = nrow(anchor_positions))
    for (i in 1:nrow(anchor_positions)) {
        current <- positions[which(positions$Document == document_names[i]),]
        temp <- ggplot2::ggplot(current,
                                ggplot2::aes(x = Dim_1,y = Dim_2))  +
            ggplot2::ggtitle(document_names[i]) +
            ggplot2::xlab("") + ggplot2::ylab("") +
            ggplot2::xlim(min(positions[,1]), max(positions[,1])) +
            ggplot2::ylim(min(positions[,2]), max(positions[,2]))

        if (is.null(decision_colors)) {
            temp <- temp + ggplot2::geom_point(shape = 19,color = colors[i])
        } else {
            temp <- temp + ggplot2::geom_point(shape = 19,color = colors)
        }
        # ggplot_list[[i]] <- temp
        ggplot_list[[i]] <- ggplot2::ggplotGrob(temp)
        # ggplot_list[[i]] <- grid::grid.force(ggplot_list[[i]])

    }

    names(ggplot_list) <- document_names

    return(ggplot_list)
}

