#' Produce a heatmap of variables in a grid layout.
#'
#' This function plots a heatmap of variables in a grid layout, optionally grouping them.
#'
#' @param data A data frame containing the data to be plotted.
#' @param value A column of `data`, containing the values that vary over the space which produces the colours.
#' @param x_axis The column of `data` to use as the x axis data.
#' @param y_axis The column of `data` to use as the y axis data.
#' @param grouping An optional grouping variable to facet the plot by.
#' @param raster Logical (default: `TRUE`). If `TRUE` uses [ggplot2::geom_raster()] for speed. Will not work if the grid is irregular.
#' @param smooth Logical (default: `FALSE`). If `raster` is `TRUE`, interpolation can be applied across the grid to obtain a smoothed grid. Ignored if `raster` is `FALSE`.
#' @param palette Colour palette to use. By default it will use the `viridis` (colour-blind friendly) palette. Other palettes available can be seen with [grDevices::hcl.pals()].
#' @param ... Other arguments passed to [`facet_wrap()`]
#'
#' @importFrom ggplot2 ggplot aes geom_tile geom_raster scale_fill_gradientn scale_x_continuous scale_y_continuous facet_wrap vars theme_bw
#' @importFrom rlang ensym enquo quo_is_null
#'
#' @return A `ggplot2` object.
#' @export
#'
#' @examples
#'
#' set.seed(42)
#' dat <- expand.grid(x = 1:5, y = 1:6)
#' dat$value <- rnorm(30)
#' dat$groups <- sample(rep(LETTERS[1:6], times = 5))
#'
#' heat_map(dat, value, x, y)
#'
#' # Column names can be quoted, but don't need to be.
#' heat_map(dat, "value", "x", "y", "groups")
#'
#' # Different palettes are available
#' heat_map(dat, value, x, y, palette = "Spectral")
#'
#' # Arguments in ... are passed through to facet_wrap
#' heat_map(dat, value, x, y, groups, labeller = ggplot2:::label_both)
#' heat_map(dat, value, x, y, groups, scales = "free_y")
#' heat_map(dat, value, x, y, groups, nrow = 1)
heat_map <- function(data, value, x_axis, y_axis, grouping = NULL, raster = TRUE, smooth = FALSE, palette = "default", ...) {

    # TODO:
    # - Error and sanity checking
    if(!is.data.frame(data)) {
        stop(data, " is not a data frame.", call. = FALSE)
    }

    rlang::check_dots_used()

    value <- rlang::ensym(value)
    x_axis <- rlang::ensym(x_axis)
    y_axis <- rlang::ensym(y_axis)
    grouping <- rlang::enquo(grouping)

    # Set the default palette to viridis
    if(palette=="default") {
        palette <- "viridis"
    }

    # rlang::check_dots_used()

    plt <- ggplot2::ggplot(data, ggplot2::aes(x = {{ x_axis }}, y = {{ y_axis }}, fill = {{ value }}))

    if(raster) {
        plt <- plt + ggplot2::geom_raster(interpolate = smooth)
    }
    else {
        plt <- plt + ggplot2::geom_tile()
    }

    plt <- plt + ggplot2::scale_fill_gradientn(colors = grDevices::hcl.colors(10, palette = palette)) +
        ggplot2::scale_x_continuous(expand = c(0, 0)) +
        ggplot2::scale_y_continuous(expand = c(0, 0))

    if(!rlang::quo_is_null(grouping)) {
        grouping <- rlang::ensym(grouping)
        plt <- plt + ggplot2::facet_wrap(ggplot2::vars({{ grouping }}), ...)
    }

    plt <- plt+ggplot2::theme_bw()
    return(plt)
}
