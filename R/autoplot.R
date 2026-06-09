#' Generate automatic plots for objects generated in biometryassist
#'
#' @param object An object to create a plot for. Currently objects from the [multiple_comparisons()] or [design()] functions with class "mct" or "design" respectively are supported.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar. Values > 1 are interpreted as the actual value above the upper error bar.
#' @param size Increase or decrease the text size within the plot for treatment labels. Numeric with default value of 4.
#' @param rotation Rotate the x axis labels and the treatment group labels within the plot. Allows for easier reading of long axis or treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param axis_rotation Enables rotation of the x axis independently of the group labels within the plot.
#' @param label_rotation Enables rotation of the treatment group labels independently of the x axis labels within the plot.
#' @param type A string specifying the type of plot to display. The default of 'point' will display a point estimate with error bars. The alternative, 'column' (or 'col'), will display a column graph with error bars.
#' @param include_errorbar Logical (default 'TRUE') indicating whether to include errorbars when plotting the predicted values from a multiple comparisons test
#' @param include_lettering Logical (default 'TRUE') indicating whether to include group lettering when plotting the predicted values from a multiple comparisons test
#' @param errorbar_type A character (default is "ci") that indicates what the errorbars in the plot represent. Current options are 95% confidence interval ("ci") or Tukeys (average) HSD value ("hsd")
#' @param trans_scale Logical (default 'FALSE') that indicates whether the predicted values should be displayed on the transformed scale.
#' @param margin Logical (default `FALSE`). A value of `FALSE` will expand the plot to the edges of the plotting area i.e. remove white space between plot and axes.
#' @param palette A string specifying the colour scheme to use for plotting or a vector of custom colours to use as the palette. Default is equivalent to "Spectral". Colour blind friendly palettes can also be provided via options `"colour blind"` (or `"colour blind"`, both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`, `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. Other palettes from [scales::brewer_pal()] are also possible.
#' @param row A variable to plot a column from `object` as rows.
#' @param column A variable to plot a column from `object` as columns.
#' @param block A variable to plot a column from `object` as blocks.
#' @param treatments A variable to plot a column from `object` as treatments.
#' @param legend Logical (default `TRUE`). If `TRUE`, displays the legend for treatment colours.
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
#' @importFrom ggplot2 autoplot ggplot aes geom_errorbar geom_text geom_point geom_line geom_col theme_bw labs theme element_text facet_wrap scale_x_discrete scale_y_continuous sec_axis
#' @importFrom rlang ensym check_dots_used
#' @importFrom stats as.formula
#' @export
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- multiple_comparisons(dat.aov, classify = "Species")
#' autoplot(output, label_height = 0.5)
#'
#' # Join the means with a line
#' autoplot(output, type = "line", label_height = 0.5)
#'
#' # Show a single Tukey's HSD reference bar instead of per-mean intervals
#' autoplot(output, errorbar_type = "hsd")
autoplot.mct <- function(
	object,
	size = 4,
	label_height = 0.1,
	rotation = 0,
	axis_rotation = rotation,
	label_rotation = rotation,
	type = "point",
	errorbar_type = "ci",
	include_errorbar = TRUE,
	include_lettering = TRUE,
	trans_scale = FALSE,
	...
) {
	stopifnot(inherits(object, "mct"))

	rlang::check_dots_used()

	type <- match.arg(
		tolower(type),
		c("point", "line", "col", "column", "bar")
	)
	errorbar_type <- match.arg(tolower(errorbar_type), c("ci", "hsd"))

	# Extract the predictions data frame from the mct object
	# For new structure: object is a list with $predictions
	# For backward compatibility: also handle old structure where object is a data frame
	if (is.list(object) && "predictions" %in% names(object)) {
		pred_df <- object$predictions
	} else {
		# Backward compatibility: object is already the data frame
		pred_df <- as.data.frame(object)
	}

	# The classify factor columns are those before "predicted.value".
	pv_pos <- match("predicted.value", colnames(pred_df))
	factor_cols <- colnames(pred_df)[seq_len(pv_pos - 1)]

	# If `by` was used (recorded as an attribute), it becomes the default
	# faceting variable(s) and the remaining classify factor(s) form the x-axis.
	# Otherwise the first factor is the x-axis and any remaining factors facet.
	by_attr <- attributes(object)$by
	if (!is.null(by_attr)) {
		x_cols <- setdiff(factor_cols, by_attr)
		x_var <- x_cols[1]
		facet_cols <- c(x_cols[-1], by_attr)
	} else {
		x_var <- factor_cols[1]
		facet_cols <- factor_cols[-1]
	}
	classify <- rlang::ensym(x_var)

	# Get ylab as attribute (works for both old and new structure)
	ylab <- attributes(object)$ylab

	# Was a back-transformation applied in multiple_comparisons()?
	has_backtrans <- "PredictedValue" %in% colnames(pred_df)

	# An HSD bar is a constant width only on the model (transformed) scale, so
	# requesting one forces plotting on that scale. Otherwise `trans_scale`
	# decides: FALSE shows the interpretable back-transformed means (when
	# available), TRUE shows the model-scale means.
	use_hsd <- errorbar_type == "hsd" && include_errorbar
	model_scale <- trans_scale || use_hsd

	# Validate HSD availability up-front: a single, constant HSD value is only
	# produced for Tukey's comparisons without a `by` split.
	hsd <- if (is.list(object) && "hsd" %in% names(object)) {
		object$hsd
	} else {
		attributes(object)$HSD
	}
	if (use_hsd && (is.null(hsd) || !is.numeric(hsd) || length(hsd) != 1)) {
		stop(
			"`errorbar_type = \"hsd\"` requires a single Honest Significant ",
			"Difference value, which is only available for Tukey's comparisons ",
			"with a constant critical value (no `by` grouping). Use ",
			"`errorbar_type = \"ci\"` instead.",
			call. = FALSE
		)
	}

	yval <- if (has_backtrans && !model_scale) {
		"PredictedValue"
	} else {
		"predicted.value"
	}
	yval <- rlang::ensym(yval)

	# Calculate hjust based on axis rotation
	hjust_value <- if (axis_rotation %% 360 == 90) {
		1
	} else if (axis_rotation %% 360 == 270 || axis_rotation %% 360 == -90) {
		0
	} else {
		0.5
	}

	plot <- ggplot2::ggplot(data = pred_df, ggplot2::aes(x = {{ classify }})) +
		ggplot2::theme_bw() +
		ggplot2::theme(
			axis.text.x = ggplot2::element_text(
				angle = axis_rotation,
				vjust = 0.5,
				hjust = hjust_value,
				...
			)
		) +
		ggplot2::labs(x = "", y = paste0("Predicted ", ylab))

	if (type %in% c("point", "line")) {
		plot <- plot +
			ggplot2::geom_point(
				ggplot2::aes(y = {{ yval }}),
				colour = "black",
				shape = 16,
				size = 2
			)
		if (type == "line") {
			plot <- plot +
				ggplot2::geom_line(
					ggplot2::aes(y = {{ yval }}, group = 1),
					colour = "black",
					linewidth = 0.4
				)
		}
	} else if (type %in% c("col", "column", "bar")) {
		plot <- plot +
			ggplot2::geom_col(
				ggplot2::aes(y = {{ yval }}),
				colour = "black",
				fill = "cornflowerblue",
				alpha = 0.75
			)
	}

	if (include_errorbar && errorbar_type == "ci") {
		if (model_scale) {
			# CI on the model scale: predicted.value +/- ci. Uses the `ci` column
			# so the interval respects the `int.type` chosen in
			# multiple_comparisons() (rather than assuming +/- 2 SE).
			plot <- plot +
				ggplot2::geom_errorbar(
					ggplot2::aes(
						ymin = .data[["predicted.value"]] - .data[["ci"]],
						ymax = .data[["predicted.value"]] + .data[["ci"]]
					),
					width = 0.2
				)
		} else {
			# Back-transformed (or untransformed) scale: low/up are already on the
			# correct scale.
			plot <- plot +
				ggplot2::geom_errorbar(
					ggplot2::aes(ymin = .data[["low"]], ymax = .data[["up"]]),
					width = 0.2
				)
		}
	} else if (include_errorbar && errorbar_type == "hsd") {
		# A single reference bar of total length `hsd` (the minimum significant
		# difference). It is given its own dedicated category to the left of the
		# treatments - rather than sharing the first mean's position - and centred
		# on the mid-point of the model-scale y range, so it reads as a
		# free-floating reference rather than belonging to any one mean.
		x_levels <- if (is.factor(pred_df[[x_var]])) {
			levels(pred_df[[x_var]])
		} else {
			sort(unique(as.character(pred_df[[x_var]])))
		}
		# Keep only levels actually present so empty categories don't appear.
		x_levels <- x_levels[x_levels %in% as.character(pred_df[[x_var]])]

		hsd_label <- "HSD"
		y_mid <- mean(range(pred_df[["predicted.value"]], na.rm = TRUE))
		hsd_df <- data.frame(
			x = hsd_label,
			ymin = y_mid - 0.5 * hsd,
			ymax = y_mid + 0.5 * hsd,
			stringsAsFactors = FALSE
		)
		names(hsd_df)[1] <- x_var
		plot <- plot +
			ggplot2::geom_errorbar(
				data = hsd_df,
				mapping = ggplot2::aes(
					x = .data[[x_var]],
					ymin = .data[["ymin"]],
					ymax = .data[["ymax"]]
				),
				width = 0.2,
				inherit.aes = FALSE
			) +
			# Reserve the left-most slot for the HSD bar; treatments follow it.
			ggplot2::scale_x_discrete(limits = c(hsd_label, x_levels))
	}

	if (include_lettering && "groups" %in% colnames(pred_df)) {
		# Position labels relative to the bounds of the scale actually being
		# plotted, so they sit just above the means whichever scale is in use.
		if (model_scale) {
			upper <- pred_df[["predicted.value"]] + pred_df[["ci"]]
			lower <- pred_df[["predicted.value"]] - pred_df[["ci"]]
		} else {
			upper <- pred_df[["up"]]
			lower <- pred_df[["low"]]
		}
		y_pos <- ifelse(upper > lower, upper, lower)
		nudge_val <- ifelse(
			abs(label_height) <= 1,
			abs(upper - lower) * label_height,
			label_height
		)

		plot <- plot +
			ggplot2::geom_text(
				ggplot2::aes(y = y_pos, label = .data[["groups"]]),
				nudge_y = nudge_val,
				size = size,
				angle = label_rotation,
				...
			)
	}

	if (length(facet_cols) > 0) {
		plot <- plot +
			ggplot2::facet_wrap(stats::as.formula(paste(
				"~",
				paste(facet_cols, collapse = " + ")
			)))
	}

	# When plotting on the model scale and a back-transformation is available,
	# add an exact (nonlinear) back-transformed secondary axis. `back_transform()`
	# is the same helper multiple_comparisons() uses to build PredictedValue.
	trans <- attributes(object)$trans
	if (model_scale && !is.null(trans)) {
		offset <- attributes(object)$offset
		power <- attributes(object)$power
		plot <- plot +
			ggplot2::scale_y_continuous(
				sec.axis = ggplot2::sec_axis(
					transform = function(z) back_transform(z, trans, offset, power),
					name = paste0("Predicted ", ylab, " (back-transformed)")
				)
			)
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
autoplot.design <- function(
	object,
	rotation = 0,
	size = 4,
	margin = FALSE,
	palette = "default",
	row = NULL,
	column = NULL,
	block = NULL,
	treatments = NULL,
	legend = TRUE,
	...
) {
	stopifnot(inherits(object, "design"))
	rlang::check_dots_used()

	if (inherits(object, "list")) {
		object <- object$design
	}

	# Handle column name expressions (existing code)
	row_expr <- rlang::enquo(row)
	column_expr <- rlang::enquo(column)
	block_expr <- rlang::enquo(block)
	trt_expr <- rlang::enquo(treatments)

	if (rlang::quo_is_null(row_expr)) {
		row_expr <- rlang::sym("row")
	}
	if (rlang::quo_is_null(column_expr)) {
		column_expr <- rlang::sym("col")
	}
	if (rlang::quo_is_null(block_expr)) {
		block_expr <- rlang::sym("block")
	}
	if (rlang::quo_is_null(trt_expr)) {
		trt_expr <- rlang::sym("treatments")
	}

	row_expr <- rlang::quo_name(row_expr)
	column_expr <- rlang::quo_name(column_expr)
	block_expr <- rlang::quo_name(block_expr)
	trt_expr <- rlang::quo_name(trt_expr)

	# Set up treatments and colours
	has_buffers <- "buffer" %in% as.character(object[[trt_expr]])

	if (has_buffers) {
		# Separate treatments and buffers for proper ordering
		treatments_only <- unique(as.character(object[[trt_expr]]))
		treatments_only <- treatments_only[treatments_only != "buffer"]
		treatments_sorted <- stringi::stri_sort(treatments_only, numeric = TRUE)

		# Set factor levels with treatments first, then buffer at the end
		factor_levels <- c(treatments_sorted, "buffer")
		object[[trt_expr]] <- factor(
			as.character(object[[trt_expr]]),
			levels = factor_levels
		)
		ntrt <- length(treatments_sorted) # Number of actual treatments (excluding buffer)
	} else {
		# Original logic for designs without buffers
		object[[trt_expr]] <- factor(
			as.character(object[[trt_expr]]),
			levels = unique(stringi::stri_sort(
				as.character(object[[trt_expr]]),
				numeric = TRUE
			))
		)
		ntrt <- nlevels(object[[trt_expr]])
	}

	# Colour palette setup
	colour_palette <- setup_colour_palette(palette, ntrt)

	# Check if buffers exist and adjust palette
	if ("buffer" %in% levels(object[[trt_expr]])) {
		colour_palette <- c(colour_palette, "white")
	}

	# Text colour setup
	colours <- data.frame(
		treatments = levels(object[[trt_expr]]),
		text_col = ifelse(is_light_colour(colour_palette), "black", "white")
	)
	colnames(colours)[1] <- trt_expr
	object <- merge(object, colours)

	# Create plot based on whether blocks exist
	if (!any(grepl("block", tolower(names(object))))) {
		plt <- create_basic_plot(
			object,
			row_expr,
			column_expr,
			trt_expr,
			rotation,
			size,
			...
		)
	} else {
		plt <- create_blocked_plot(
			object,
			row_expr,
			column_expr,
			block_expr,
			trt_expr,
			rotation,
			size,
			...
		)
	}

	# Apply styling
	plt <- plt +
		scale_fill_manual(
			values = colour_palette,
			name = tools::toTitleCase(trt_expr)
		)

	# Control legend visibility
	if (!legend) {
		plt <- plt + ggplot2::theme(legend.position = "none")
	}

	plt <- apply_axis_styling(plt, margin, object, row_expr, column_expr)

	return(plt)
}


create_basic_plot <- function(
	object,
	row_expr,
	column_expr,
	trt_expr,
	rotation,
	size,
	...
) {
	# Separate buffer plots from treatment plots
	buffer_plots <- object[object[[trt_expr]] == "buffer", ]
	treatment_plots <- object[object[[trt_expr]] != "buffer", ]

	ggplot2::ggplot() +
		ggplot2::geom_tile(
			data = object,
			mapping = ggplot2::aes(
				x = .data[[column_expr]],
				y = .data[[row_expr]],
				fill = .data[[trt_expr]]
			),
			colour = "black"
		) +
		# Only add text to non-buffer plots
		ggplot2::geom_text(
			data = treatment_plots,
			mapping = ggplot2::aes(
				x = .data[[column_expr]],
				y = .data[[row_expr]],
				label = .data[[trt_expr]]
			),
			colour = treatment_plots$text_col,
			angle = rotation,
			size = size,
			...
		) +
		ggplot2::theme_bw()
}

create_blocked_plot <- function(
	object,
	row_expr,
	column_expr,
	block_expr,
	trt_expr,
	rotation,
	size,
	...
) {
	# Block boundary calculation
	blkdf <- calculate_block_boundaries(object, block_expr)

	# Separate buffer plots from treatment plots
	buffer_plots <- object[object[[trt_expr]] == "buffer", ]
	treatment_plots <- object[object[[trt_expr]] != "buffer", ]

	ggplot2::ggplot() +
		ggplot2::geom_tile(
			data = object,
			mapping = ggplot2::aes(
				x = .data[[column_expr]],
				y = .data[[row_expr]],
				fill = .data[[trt_expr]]
			),
			colour = "black"
		) +
		# Only add text to non-buffer plots
		ggplot2::geom_text(
			data = treatment_plots,
			mapping = ggplot2::aes(
				x = .data[[column_expr]],
				y = .data[[row_expr]],
				label = .data[[trt_expr]]
			),
			colour = treatment_plots$text_col,
			angle = rotation,
			size = size,
			...
		) +
		ggplot2::geom_rect(
			data = blkdf,
			mapping = ggplot2::aes(
				xmin = xmin,
				xmax = xmax,
				ymin = ymin,
				ymax = ymax
			),
			linewidth = 1.8,
			colour = "black",
			fill = NA
		) +
		ggplot2::geom_rect(
			data = blkdf,
			mapping = ggplot2::aes(
				xmin = xmin,
				xmax = xmax,
				ymin = ymin,
				ymax = ymax
			),
			linewidth = 0.6,
			colour = "white",
			fill = NA
		) +
		ggplot2::theme_bw()
}

apply_axis_styling <- function(plot, margin, object, row_expr, column_expr) {
	if (!margin) {
		# No margin - expand plot to edges with no white space
		plot <- plot +
			ggplot2::scale_x_continuous(
				expand = c(0, 0),
				breaks = seq(1, max(object[[column_expr]]), 1)
			) +
			ggplot2::scale_y_continuous(
				expand = c(0, 0),
				trans = scales::reverse_trans(),
				breaks = seq(1, max(object[[row_expr]]), 1)
			)
	} else {
		# With margin - default ggplot spacing
		plot <- plot +
			ggplot2::scale_x_continuous(
				breaks = seq(1, max(object[[column_expr]]), 1)
			) +
			ggplot2::scale_y_continuous(
				trans = scales::reverse_trans(),
				breaks = seq(1, max(object[[row_expr]]), 1)
			)
	}
	return(plot)
}

calculate_block_boundaries <- function(object, block_expr) {
	blkdf <- data.frame(
		block = sort(unique(object[[block_expr]])),
		xmin = 0,
		xmax = 0,
		ymin = 0,
		ymax = 0
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
