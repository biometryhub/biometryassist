#' Summary Graph
#'
#' Produce a graphical summary of variables from a data frame. Variables are plotted
#' as box plots, grouped and, if enough variables are provided, coloured and facetted.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param response The response variable to plot.
#' @param exp_var The explanatory (or grouping) variable to plot.
#' @param resp_units A string providing units to display on the response variable axis.
#'
#' @return A ggplot2 plot
#' @export
#'
#' @examples
#'
#' summary_graph(iris, "Petal.Length", "Species", "mm")
#' summary_graph(npk, "yield", c("N", "P", "K"), "lb/plot")
#'
summary_graph <- function(data, response, exp_var, resp_units){

    # TODO: NSE

    if(!is.data.frame(data)) {
        stop(data, " is not a data frame.", call. = FALSE)
    }
    if(response %!in% colnames(data)) {
        stop(response, " does not appear to be a column of data. Please check input.", call. = FALSE)
    }
    if(!is.numeric(data[[response]])) {
        warning(response, " is not a numeric variable.")
    }

    if(length(exp_var)==1) {
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[exp_var]], y = .data[[response]])) +
            ggplot2::geom_boxplot()
    }
    else if(length(exp_var) == 2) {
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[exp_var[1]]], y = .data[[response]],
                                                        colour = .data[[exp_var[2]]], group = .data[[exp_var[2]]])) +
            ggplot2::stat_summary(fun = mean, geom = "point") +
            ggplot2::stat_summary(fun = mean, geom = "line") +
            ggplot2::geom_point(alpha = 0.3)
    }
    else if(length(exp_var)==3) {
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[exp_var[1]]], y = .data[[response]],
                                                        colour = .data[[exp_var[2]]], group = .data[[exp_var[2]]])) +
            ggplot2::stat_summary(fun = mean, geom = "point") +
            ggplot2::stat_summary(fun = mean, geom = "line") +
            ggplot2::facet_wrap(~ .data[[exp_var[3]]])
    }
    else {
        stop("Additional explanatory variables are not currently handled.")
    }

    gg <- gg + ggplot2::geom_point(alpha = 0.3) +
        ggplot2::labs(y = paste(response, " (", resp_units, ")", sep = "")) +
        ggplot2::theme_bw()

    return(gg)
}
