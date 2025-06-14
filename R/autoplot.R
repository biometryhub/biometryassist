#' Generate automatic plots for objects generated in biometryassist
#'
#' @param object An object to create a plot for. Currently objects from the [multiple_comparisons()] or [design()] functions with class "mct" or "design" respectively are supported.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar. Values > 1 are interpreted as the actual value above the upper error bar.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param rotation Rotate the x axis labels and the treatment group labels within the plot. Allows for easier reading of long axis or treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param axis_rotation Enables rotation of the x axis independently of the group labels within the plot.
#' @param label_rotation Enables rotation of the treatment group labels independently of the x axis labels within the plot.
#' @param type A string specifying the type of plot to display. The default of 'point' will display a point estimate with error bars. The alternative, 'column' (or 'col'), will display a column graph with error bars.
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param palette A string specifying the colour scheme to use for plotting or a vector of custom colours to use as the palette. Default is equivalent to "Spectral". Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"colour blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Other palettes from [scales::brewer_pal()] are also possible.
#' @param row A variable to plot a column from `object` as rows.
#' @param column A variable to plot a column from `object` as columns.
#' @param block A variable to plot a column from `object` as blocks.
#' @param treatments A variable to plot a column from `object` as treatments.
#' @inheritParams rlang::args_dots_used
#'
#' @name autoplot
#'
#' @returns A `ggplot2` object.
#' @seealso [multiple_comparisons()] and [design()]
#'
NULL

#' @rdname autoplot
#' @importFrom ggplot2 autoplot
#' @export
ggplot2::autoplot


#' @rdname autoplot
#' @importFrom ggplot2 autoplot ggplot aes geom_errorbar geom_text geom_point theme_bw labs theme element_text facet_wrap
#' @importFrom rlang ensym check_dots_used
#' @importFrom stats as.formula
#' @export
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- multiple_comparisons(dat.aov, classify = "Species")
#' autoplot(output, label_height = 0.5)
autoplot.mct <- function(object, size = 4, label_height = 0.1,
                         rotation = 0, axis_rotation = rotation,
                         label_rotation = rotation, type = "point", ...) {
    stopifnot(inherits(object, "mct"))

    rlang::check_dots_used()
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
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = axis_rotation, ...)) +
        ggplot2::labs(x = "", y = paste0("Predicted ", ylab))

    if(type == "point") {
        plot <- plot + ggplot2::geom_point(ggplot2::aes(y = {{ yval }}), colour = "black", shape = 16, size = 2) +
            ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2)
    }
    else if(type %in% c("col", "column")) {
        plot <- plot + ggplot2::geom_col(ggplot2::aes(y = {{ yval }}), colour = "black", fill = "cornflowerblue", alpha = 0.75) +
            ggplot2::geom_errorbar(aes(ymin = low, ymax = up), width = 0.2)
    }

    if("groups" %in% colnames(object)) {
        plot <- plot +
            ggplot2::geom_text(ggplot2::aes(x = {{ classify }}, y = ifelse(object$up > object$low, object$up, object$low),
                                            label = object$groups),
                               nudge_y = ifelse(abs(label_height) <= 1,
                                                abs(object$up-object$low)*label_height, # invert for cases with inverse transform
                                                label_height),
                               size = size, angle = label_rotation, ...)
    }

    if(exists("classify3")) {
        plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", classify2, "+", classify3)))
    }
    else if(exists("classify2")) {
        plot <- plot + ggplot2::facet_wrap(stats::as.formula(paste("~", classify2)))
    }
    return(plot)
}


#' @rdname autoplot
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom ggplot2 ggplot geom_tile aes geom_text theme_bw scale_fill_manual scale_x_continuous scale_y_continuous scale_y_reverse
#' @importFrom scales brewer_pal reverse_trans viridis_pal
#' @importFrom stringi stri_sort
#' @importFrom rlang check_dots_used enquo sym quo_is_null quo_name
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
#'
#' # Custom colour palette
#' autoplot(des.out, palette = c("#ef746a", "#3fbfc5", "#81ae00", "#c37cff"))
#'
#' # Visualise different components of a split plot design
#' des.out <- design(type = "split", treatments = c("A", "B"), sub_treatments = 1:4,
#' reps = 4, nrows = 8, ncols = 4, brows = 4, bcols = 2, seed = 42)
#'
#' # Show the wholeplot components
#' autoplot(des.out, treatments = wholeplots)
#'
#' # Display block level
#' autoplot(des.out, treatments = block)
autoplot.design <- function(object, rotation = 0, size = 4,
                            margin = FALSE, palette = "default",
                            row = NULL, column = NULL, block = NULL,
                            treatments = NULL, ...) {
    stopifnot(inherits(object, "design"))
    rlang::check_dots_used()

    if(inherits(object, "list")) {
        object <- object$design
    }

    # Handle column name expressions (existing code)
    row_expr <- rlang::enquo(row)
    column_expr <- rlang::enquo(column)
    block_expr <- rlang::enquo(block)
    trt_expr <- rlang::enquo(treatments)

    if(rlang::quo_is_null(row_expr)) row_expr <- rlang::sym("row")
    if(rlang::quo_is_null(column_expr)) column_expr <- rlang::sym("col")
    if(rlang::quo_is_null(block_expr)) block_expr <- rlang::sym("block")
    if(rlang::quo_is_null(trt_expr)) trt_expr <- rlang::sym("treatments")

    row_expr <- rlang::quo_name(row_expr)
    column_expr <- rlang::quo_name(column_expr)
    block_expr <- rlang::quo_name(block_expr)
    trt_expr <- rlang::quo_name(trt_expr)

    # Set up treatments and colours
    has_buffers <- "buffer" %in% as.character(object[[trt_expr]])

    if(has_buffers) {
        # Separate treatments and buffers for proper ordering
        treatments_only <- unique(as.character(object[[trt_expr]]))
        treatments_only <- treatments_only[treatments_only != "buffer"]
        treatments_sorted <- stringi::stri_sort(treatments_only, numeric = TRUE)

        # Set factor levels with treatments first, then buffer at the end
        factor_levels <- c(treatments_sorted, "buffer")
        object[[trt_expr]] <- factor(as.character(object[[trt_expr]]), levels = factor_levels)
        ntrt <- length(treatments_sorted)  # Number of actual treatments (excluding buffer)
    } else {
        # Original logic for designs without buffers
        object[[trt_expr]] <- factor(as.character(object[[trt_expr]]),
                                     levels = unique(stringi::stri_sort(as.character(object[[trt_expr]]), numeric = TRUE)))
        ntrt <- nlevels(object[[trt_expr]])
    }

    # Colour palette setup
    colour_palette <- setup_colour_palette(palette, ntrt)

    # Check if buffers exist and adjust palette
    if("buffer" %in% levels(object[[trt_expr]])) {
        colour_palette <- c(colour_palette, "white")
    }

    # Text colour setup
    colours <- data.frame(treatments = levels(object[[trt_expr]]),
                          text_col = ifelse(.is_light_colour(colour_palette), "black", "white"))
    colnames(colours)[1] <- trt_expr
    object <- merge(object, colours)


    # Create plot based on whether blocks exist
    if(!any(grepl("block", tolower(names(object))))) {
        plt <- create_basic_plot(object, row_expr, column_expr, trt_expr, rotation, size, ...)
    } else {
        plt <- create_blocked_plot(object, row_expr, column_expr, block_expr, trt_expr, rotation, size, ...)
    }

    # Apply styling
    plt <- plt + scale_fill_manual(values = colour_palette, name = tools::toTitleCase(trt_expr))

    plt <- apply_axis_styling(plt, margin, object, row_expr, column_expr)

    return(plt)
}

#' @keywords internal
setup_colour_palette <- function(palette, ntrt) {
    # Handle custom colour palettes (vector of colours)
    if(length(palette) > 1) {
        if(length(palette) != ntrt) {
            stop("palette needs to be a single string to choose a predefined palette, or ",
                 ntrt, " custom colours.")
        }
        return(palette)
    }

    # Handle single string palette names
    palette <- tolower(trimws(palette))

    # Default Spectral palette
    if(palette == "default") {
        return(grDevices::colorRampPalette(scales::brewer_pal(palette = "Spectral")(11))(ntrt))
    }

    # colourBrewer palettes
    brewer_palettes <- c("brbg", "piyg", "prgn", "puor", "rdbu", "rdgy",
                         "rdylbu", "rdylgn", "spectral", "set3", "paired")
    if(palette %in% brewer_palettes) {
        # Convert to proper case for scales::brewer_pal
        palette_proper <- switch(palette,
                                 "brbg" = "BrBG",
                                 "piyg" = "PiYG",
                                 "prgn" = "PRGn",
                                 "puor" = "PuOr",
                                 "rdbu" = "RdBu",
                                 "rdgy" = "RdGy",
                                 "rdylbu" = "RdYlBU",
                                 "rdylgn" = "RdYlGn",
                                 "spectral" = "Spectral",
                                 "set3" = "Set3",
                                 "paired" = "Paired"
        )
        return(grDevices::colorRampPalette(scales::brewer_pal(palette = palette_proper)(11))(ntrt))
    }

    # colour blind friendly palettes (viridis family)
    viridis_patterns <- c("colou?r([[:punct:]]|[[:space:]]?)blind", "cb", "viridis")
    if(any(sapply(viridis_patterns, function(pattern) grepl(pattern, palette, ignore.case = TRUE)))) {
        return(scales::viridis_pal(option = "viridis")(ntrt))
    }

    # Other viridis options
    viridis_options <- c("magma", "inferno", "cividis", "plasma", "rocket", "mako", "turbo")
    if(palette %in% viridis_options) {
        return(scales::viridis_pal(option = palette)(ntrt))
    }

    # If we get here, the palette name is invalid
    valid_options <- c("default", brewer_palettes, "colour blind", "colour blind",
                       "cb", viridis_options)
    stop("Invalid value for palette. Valid options are: ",
         paste(valid_options, collapse = ", "),
         ", or a vector of ", ntrt, " custom colours.", call. = FALSE)
}


create_basic_plot <- function(object, row_expr, column_expr, trt_expr, rotation, size, ...) {
    # Separate buffer plots from treatment plots
    buffer_plots <- object[object[[trt_expr]] == "buffer", ]
    treatment_plots <- object[object[[trt_expr]] != "buffer", ]

    ggplot2::ggplot() +
        ggplot2::geom_tile(data = object,
                           mapping = ggplot2::aes(x = .data[[column_expr]],
                                                  y = .data[[row_expr]],
                                                  fill = .data[[trt_expr]]),
                           colour = "black") +
        # Only add text to non-buffer plots
        ggplot2::geom_text(data = treatment_plots,
                           mapping = ggplot2::aes(x = .data[[column_expr]],
                                                  y = .data[[row_expr]],
                                                  label = .data[[trt_expr]]),
                           colour = treatment_plots$text_col, angle = rotation, size = size, ...) +
        ggplot2::theme_bw()
}

create_blocked_plot <- function(object, row_expr, column_expr, block_expr, trt_expr, rotation, size, ...) {
    # Block boundary calculation
    blkdf <- calculate_block_boundaries(object, block_expr)

    # Separate buffer plots from treatment plots
    buffer_plots <- object[object[[trt_expr]] == "buffer", ]
    treatment_plots <- object[object[[trt_expr]] != "buffer", ]

    ggplot2::ggplot() +
        ggplot2::geom_tile(data = object,
                           mapping = ggplot2::aes(x = .data[[column_expr]],
                                                  y = .data[[row_expr]],
                                                  fill = .data[[trt_expr]]),
                           colour = "black") +
        # Only add text to non-buffer plots
        ggplot2::geom_text(data = treatment_plots,
                           mapping = ggplot2::aes(x = .data[[column_expr]],
                                                  y = .data[[row_expr]],
                                                  label = .data[[trt_expr]]),
                           colour = treatment_plots$text_col, angle = rotation, size = size, ...) +
        ggplot2::geom_rect(data = blkdf,
                           mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                           linewidth = 1.8, colour = "black", fill = NA) +
        ggplot2::geom_rect(data = blkdf,
                           mapping = ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                           linewidth = 0.6, colour = "white", fill = NA) +
        ggplot2::theme_bw()
}

apply_axis_styling <- function(plot, margin, object, row_expr, column_expr) {
    if(!margin) {
        # No margin - expand plot to edges with no white space
        plot <- plot + ggplot2::scale_x_continuous(expand = c(0, 0),
                                                   breaks = seq(1, max(object[[column_expr]]), 1)) +
            ggplot2::scale_y_continuous(expand = c(0, 0),
                                        trans = scales::reverse_trans(),
                                        breaks = seq(1, max(object[[row_expr]]), 1))
    } else {
        # With margin - default ggplot spacing
        plot <- plot + ggplot2::scale_x_continuous(breaks = seq(1, max(object[[column_expr]]), 1)) +
            ggplot2::scale_y_continuous(trans = scales::reverse_trans(),
                                        breaks = seq(1, max(object[[row_expr]]), 1))
    }
    return(plot)
}

calculate_block_boundaries <- function(object, block_expr) {
    blkdf <- data.frame(
        block = sort(unique(object[[block_expr]])),
        xmin = 0, xmax = 0, ymin = 0, ymax = 0
    )

    for (i in 1:nrow(blkdf)) {
        tmp <- object[object[[block_expr]] == blkdf$block[i], ]
        blkdf[i, "ymin"] <- (min(tmp$row) - 0.5)
        blkdf[i, "ymax"] <- (max(tmp$row) + 0.5)
        blkdf[i, "xmin"] <- (min(tmp$col) - 0.5)
        blkdf[i, "xmax"] <- (max(tmp$col) + 0.5)
    }

    return(blkdf)
}
