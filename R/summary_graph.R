#' Visualise a graphical summary of variables from a data frame
#'
#' Variables are plotted in different ways according to the number of explanatory
#' variables provided as input.
#'
#' @param data A data frame containing the variables to be plotted.
#' @param response The response variable to plot.
#' @param exp_var The explanatory (or grouping) variable(s) to plot. Up to three can be provided.
#' @param resp_units A string providing units to display on the response variable (y) axis. Will use the empty string by default so axes will have no units by default.
#'
#' @importFrom ggplot2 ggplot aes geom_boxplot stat_summary facet_wrap geom_point labs theme_bw
#' @importFrom rlang ensym ensyms as_string
#'
#' @return A ggplot2 plot object
#' @export
#'
#' @details
#' With a single explanatory variable, a boxplot grouped by `exp_var` is produced.
#' With two explanatory variables, a dot-plot with lines connecting the mean of each
#' group is produced, with the first element of `exp_var` used as the x axis variable,
#' and the second is used to colour the points. Three explanatory variables produces
#' the same as two, but with the third used to facet the plot.
#'
#' @examples
#'
#' summary_graph(iris, "Petal.Length", "Species", "mm")
#'
#' # Multiple
#' summary_graph(npk, "yield", c("N", "P"), "lb/plot")
#'
#  # Three way interaction
#' summary_graph(npk, "yield", c("N", "P", "K"), "lb/plot")
#'
summary_graph <- function(data, response, exp_var, resp_units = "") {

    # TODO: NSE

    response <- rlang::ensym(response)

    if(!is.data.frame(data)) {
        stop(data, " is not a data frame.", call. = FALSE)
    }
    if(rlang::as_string(response) %!in% colnames(data)) {
        stop(rlang::as_string(response), " does not appear to be a column of data. Please check input.", call. = FALSE)
    }
    if(!is.numeric(data[[rlang::as_string(response)]])) {
        stop(rlang::as_string(response), " is not a numeric variable.")
    }
    if(!is.character(resp_units)) {
        stop("resp_units must be provided as a string with quotes.")
    }


    if(length(exp_var)==1) {
        exp_var <- rlang::ensyms(exp_var)
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = !!exp_var[[1]], y = {{ response }})) +
            ggplot2::geom_boxplot()
    }
    else if(length(exp_var) == 2) {
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[exp_var[1]]], y = {{ response }},
                                                        colour = .data[[exp_var[2]]], group = .data[[exp_var[2]]])) +
            ggplot2::stat_summary(fun = mean, geom = "point") +
            ggplot2::stat_summary(fun = mean, geom = "line")
    }
    else if(length(exp_var)==3) {
        gg <- ggplot2::ggplot(data = data, ggplot2::aes(x = .data[[exp_var[1]]], y = {{ response }},
                                                        colour = .data[[exp_var[2]]], group = .data[[exp_var[2]]])) +

            ggplot2::stat_summary(fun = mean, geom = "point") +
            ggplot2::stat_summary(fun = mean, geom = "line") +
            ggplot2::facet_wrap(~ .data[[exp_var[3]]])
    }
    else {
        stop("Additional explanatory variables are not currently supported.")
    }

    if(resp_units!="") {
        gg <- gg + ggplot2::labs(y = paste({{ response }}, " (", resp_units, ")", sep = ""))

    }
    gg <- gg + ggplot2::geom_point(alpha = 0.3) +
        ggplot2::theme_bw()

    return(gg)
}


