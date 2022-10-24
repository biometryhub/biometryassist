#' Automatic plots for objects generated in biometryassist
#'
#' @param object An object to create a plot for. Currently objects from the [multiple_comparisons()] or [design()] functions with class "mct" or "design" respectively are supported.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar. Values > 1 are interpreted as the actual value above the upper error bar.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param rotation Rotate the x axis labels and the treatment group labels within the plot. Allows for easier reading of long axis or treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param axis_rotation Enables rotation of the x axis independently of the group labels within the plot.
#' @param label_rotation Enables rotation of the treatment group labels independently of the x axis labels within the plot.
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param palette A string specifying the colour scheme to use for plotting. Default is equivalent to "Spectral". Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"color blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"` or `"cividis"`. Other palettes from [scales::brewer_pal()] are also possible.
#' @inheritParams rlang::args_dots_used
#'
#' @name autoplot
#'
#' @return A `ggplot2` object.
#' @seealso [multiple_comparisons()] and [design()]
#'
NULL

#' @rdname autoplot
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' @rdname autoplot
#' @importFrom ggplot2 autoplot ggplot aes geom_errorbar geom_text geom_point theme_bw labs theme element_text facet_wrap
#' @importFrom rlang ensym
#' @export
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- multiple_comparisons(dat.aov, classify = "Species")
#' autoplot(output, label_height = 0.5)
autoplot.mct <- function(object, size = 4, label_height = 0.1, rotation = 0, axis_rotation = rotation, label_rotation = rotation, ...) {
    stopifnot(inherits(object, "mct"))

    # classify is just the first n columns (before predicted.value)
    classify <- colnames(object)[1]
    classify <- rlang::ensym(classify)
    if(colnames(object)[2] != "predicted.value") {
        classify2 <- colnames(object)[2]
    }
    if(colnames(object)[2] != "predicted.value" & colnames(object)[3] != "predicted.value") {
        classify3 <- colnames(object)[3]
    }

    # Get ylab as attribute
    ylab <- attributes(object)$ylab

    yval <- ifelse("PredictedValue" %in% colnames(object), "PredictedValue", "predicted.value")
    yval <- rlang::ensym(yval)

    plot <- ggplot2::ggplot(data = object, ggplot2::aes(x = {{ classify }})) +
        ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2) +
        ggplot2::geom_text(ggplot2::aes(x = {{ classify }}, y = ifelse(object$up > object$low, object$up, object$low),
                                         label = object$groups),
                           nudge_y = ifelse(abs(label_height) <= 1,
                                            abs(object$up-object$low)*label_height, # invert for cases with inverse transform
                                            label_height),
                           size = size, angle = label_rotation, ...) +
        ggplot2::geom_point(ggplot2::aes(y = {{ yval }}), color = "black", shape = 16) + ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = axis_rotation, ...)) +
        ggplot2::labs(x = "", y = paste0("Predicted ", ylab))

    if(exists("classify3")) {
        plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2, "+", classify3)))
    }
    else if(exists("classify2")) {
        plot <- plot + ggplot2::facet_wrap(as.formula(paste("~", classify2)))
    }
    return(plot)
}


#' @rdname autoplot
#' @importFrom farver decode_colour
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales brewer_pal reverse_trans viridis_pal
#' @importFrom stringi stri_sort
#' @export
#' @examples
#' des.out <- design(type = "crd", treatments = c(1, 5, 10, 20),
#'                   reps = 5, nrows = 4, ncols = 5, seed = 42, plot = FALSE)
#' autoplot(des.out)
#'
#' # Colour blind friendly colours
#' autoplot(des.out, palette = "colour-blind")
#'
#' # Alternative colour scheme
#' autoplot(des.out, palette = "plasma")
autoplot.design <- function(object, rotation = 0, size = 4, margin = FALSE, palette = "default", row = NULL, col = NULL, ...) {
    stopifnot(inherits(object, "design"))

    if(inherits(object, "list")) {
        object <- object$design
    }

    ntrt <- nlevels(as.factor(object$treatments))

    # create the colours for the graph
    if(palette == "default") {
        colour_palette <- grDevices::colorRampPalette(scales::brewer_pal(palette = "Spectral")(11))(ntrt)
        object$text_col <- "black"
    }
    else if(palette %in% c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy",
                           "RdYlBu", "RdYlGn", "Spectral", "Set3", "Paired")) {
        colour_palette <- grDevices::colorRampPalette(scales::brewer_pal(palette = palette)(11))(ntrt)
        object$text_col <- "black"
    }
    else if(any(grepl("(colou?r([[:punct:]]|[[:space:]]?)blind)|cb|viridis", palette, ignore.case = T))) {
        colour_palette <- scales::viridis_pal(option = "viridis")(ntrt)
        # Set text colour to be light on dark colours
        hcl <- farver::decode_colour(colour_palette, "rgb", "hcl")
        cols <- data.frame(treatments = levels(as.factor(object$treatments)),
                           text_col = ifelse(hcl[, "l"] > 50, "black", "white"))
        object <- merge(object, cols)
    }
    else if(tolower(trimws(palette)) %in% c("magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo")) {
        colour_palette <- scales::viridis_pal(option = palette)(ntrt)
        # Set text colour to be light on dark colours
        hcl <- farver::decode_colour(colour_palette, "rgb", "hcl")
        cols <- data.frame(treatments = levels(as.factor(object$treatments)),
                           text_col = ifelse(hcl[, "l"] > 50, "black", "white"))
        object <- merge(object, cols)
    }
    else {
        stop("Invalid value for palette.")
    }

    if(!any(grepl("block", names(object)))) {
        # create the graph
        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = col, y = row, label = treatments), colour = object$text_col, angle = rotation, size = size) +
            ggplot2::theme_bw()
    }
    else {
        # Set up dataframe with coordinates for drawing the blocks
        blkdf <- data.frame(
            block = sort(unique(object$block)),
            xmin = 0, xmax = 0, ymin = 0, ymax = 0
        )
        for (i in 1:nrow(blkdf)) {
            item <- blkdf$block[i]
            tmp <- object[object$block == item, ]
            blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
            blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
            blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
            blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
        }

        plt <- ggplot2::ggplot() +
            ggplot2::geom_tile(data = object, mapping = ggplot2::aes(x = col, y = row, fill = treatments), colour = "black") +
            ggplot2::geom_text(data = object, mapping = ggplot2::aes(x = col, y = row, label = treatments), colour = object$text_col, angle = rotation, size = size) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 1.8, colour = "black", fill = NA
            ) +
            ggplot2::geom_rect(
                data = blkdf,
                mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                size = 0.6, colour = "white", fill = NA
            ) +
            ggplot2::theme_bw()
    }

    plt <- plt + scale_fill_manual(values = colour_palette, name = "Treatment")

    if(!margin) {
        plt <- plt + ggplot2::scale_x_continuous(expand = c(0, 0), breaks = seq(1, max(object$col), 1)) + ggplot2::scale_y_continuous(expand = c(0, 0), trans = scales::reverse_trans(), breaks = seq(1, max(object$row), 1))
    }
    else {
        plt <- plt + ggplot2::scale_x_continuous(breaks = seq(1, max(object$col), 1))+ ggplot2::scale_y_continuous(trans = scales::reverse_trans(), breaks = seq(1, max(object$row), 1))
    }

    return(plt)
}



