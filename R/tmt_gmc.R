#' Function for the Graphical Model Check
#' 
#' This function performs a so-called graphical model check on the basis of the previously performed Likelihood Ratio Test [tmt::tmt_lrttest()]. The estimated item parameters of the two groups are plotted against each other. There is the possibility in this function to highlight items, to be excluded items from the plot, and to produce confidence-ellipses if desired.
#'
#' @param object      object of the function [tmt::tmt_lrttest()]
#' @param title       of the plot
#' @param xaxis       description of the x-axis
#' @param yaxis       description of the y-axis
#' @param lim         of the plot
#' @param ellipse     should confidence-ellipse be plotted
#' @param drop        which items should be excluded from the plot
#' @param alpha       which alpha should be used for the ellipse
#' @param legendtitle Title of the Legend
#' @param info        vector with further information for the Plot with names of submitted items
#'
#' @author Jan Steinfeld
#'
#' @importFrom stats qnorm 
#' @importFrom ggplot2 ggplot aes geom_abline geom_point geom_text theme_minimal labs ggtitle ylab xlab theme element_text scale_fill_brewer geom_path
#' @importFrom rlang .data
#' 
#' @example ./R/.example_gmc.R
#' 
#' @export
tmt_gmc <- function (object,
                 title = "graphical model check",
                 xaxis = NULL,
                 yaxis = NULL,
                 lim = NULL,
                 ellipse = FALSE,
                 drop = NULL,
                 alpha = 0.05,
                 legendtitle = "split criteria",
                 info = NULL)
{
 # ------------------------------------------------------
 # check input values
    if (!inherits(object, c("lrtest_mst","lrtest_nmst"))) {
        stop("Only objects of type 'lrtest_mst' or 'lrtest_nmst' are allowed. Pleas use the tmt_lrtest function first.\n")
    }
    if (is.null(xaxis)) {
      xaxis <- paste("item parameter for group: ", names(object$betapars_subgroup)[1], sep = "")
    }
    if (is.null(yaxis)) {
      yaxis <- paste("item parameter for group: ", names(object$betapars_subgroup)[2], sep = "")
    }
    if(!is.null(info)) info <- info[!names(info)%in%drop]
 # ------------------------------------------------------
    subgroup_1 <- object$betapars_subgroup[[1]]
    subgroup_2 <- object$betapars_subgroup[[2]]
    names(subgroup_1) <- gsub("est.b_","", names(subgroup_1))
    names(subgroup_2) <- gsub("est.b_","", names(subgroup_2))


    if (!is.null(drop)) {
        subgroup_1 <- subgroup_1[!names(subgroup_1) %in% drop]
        subgroup_2 <- subgroup_2[!names(subgroup_2) %in% drop]
    }

    if (!is.null(info)) {
        subgroup_1 <- subgroup_1[names(subgroup_1) %in% names(info)]
        subgroup_2 <- subgroup_2[names(subgroup_2) %in% names(info)]
    }

    daten <- data.frame(
                Items = as.character(names(subgroup_1)),
                subg1 = as.numeric(subgroup_1),
                seg1 = NA,
                subg2 = as.numeric(subgroup_2),
                seg2 = NA,
                info = NA)

    if(!is.null(info)){
        daten$info <- info[match(daten$Items,names(info))]
    }


    if (!is.null(object$se.beta_subgroup[[1]])) {
        segroup_1 <- object$se.beta_subgroup[[1]]
        segroup_2 <- object$se.beta_subgroup[[2]]
        names(segroup_1) <- gsub("se.b_","", names(segroup_1))
        names(segroup_2) <- gsub("se.b_","", names(segroup_2))
        daten$seg1 <- segroup_1[match(daten$Items,names(segroup_1))]
        daten$seg2 <- segroup_2[match(daten$Items,names(segroup_2))]
    }

    if (ellipse) {
        if (is.null(object$se.beta_subgroup[[1]])) {
            warning("There are no standard error in your data. Pleas set 'se = TRUE' in tmt_lrtest.")
            ellipse <- FALSE
        }else{
            x <- daten$subg1
            y <- daten$subg2
            se_x <- daten$seg1
            se_y <- daten$seg2
            z <- stats::qnorm(((1 - alpha) + 1)/2)
            se_x1 <- x + z * se_x
            se_x2 <- x - z * se_x
            se_y1 <- y + z * se_y
            se_y2 <- y - z * se_y

            tmp <- list()
           for (i in seq_len(nrow(daten))) {
                tmp[[i]] <- data.frame(Items = rep(daten$Items[i], each = 301),
                                       draw_ellipse(x = x[[i]],
                                                    y = y[[i]],
                                                    a = abs(x[[i]] - (x[[i]] + z * se_x[[i]])),
                                                    b = abs(y[[i]] - (y[[i]] + z * se_y[[i]])),
                                                    angle = 0,
                                                    n = 300))
            }
            ellipse_i <- do.call(rbind, tmp)
            tmp <- list()
            for (i in seq_len(nrow(daten))) {
                tmp[[i]] <- data.frame(x = c(se_x1[[i]], se_x2[[i]],x[[i]], x[[i]]),
                                       y = c(y[[i]], y[[i]], se_y1[[i]],se_y2[[i]]))
            }
            line_i <- do.call(rbind,tmp)
        }
    }

  p1 <- ggplot2::ggplot(daten, ggplot2::aes(x = .data[["subg1"]], y = .data[["subg2"]])) +
    ggplot2::geom_abline(intercept = 0, slope = 1, linewidth  = 1.2) +
    ggplot2::geom_point(data = daten, mapping = ggplot2::aes(fill = .data[["info"]]), shape = 21, colour = "#000000" , size = 6) +
    ggplot2::geom_text(data = daten, mapping = ggplot2::aes(label = .data[["Items"]], vjust = 0.4), color = "black", size = 2) +
    ggplot2::theme_minimal() + 
    ggplot2::labs(fill=legendtitle) +
    ggplot2::ggtitle(title) +
    ggplot2::ylab(yaxis) +
    ggplot2::xlab(xaxis) +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::scale_fill_brewer(palette = "Set2")


if (ellipse) {
    if(all(daten[1,c("subg1","subg2")]==0)){
        ellipse_i <- ellipse_i[ellipse_i[,"Items"]!=daten[1,"Items"],]
    }
        for(i in unique(ellipse_i$Items)){
            p1 <- p1 + ggplot2::geom_path(mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]), 
                                          data = ellipse_i[ellipse_i$Items==i, ], 
                                          colour = "dimgray")
        }
        for (ii in seq(3, (4 * length(x) - 1), by = 2)) {
            p1 <- p1 + ggplot2::geom_path(mapping = ggplot2::aes(x = .data[["x"]], y = .data[["y"]]), 
                                          data = line_i[ii:(ii + 1), ], 
                                          colour = "dimgray", 
                                          linetype = "dashed")
        }
}
p1
}
