equivalent_ggplot2 <- function(x, y) {
	# Create temporary files that will be automatically cleaned up when the function exits
	# Determine file extension based on svglite availability
	fileext <- ifelse(rlang::is_installed("svglite"), ".svg", ".png")
	tmp1 <- withr::local_tempfile(fileext = fileext)
	tmp2 <- withr::local_tempfile(fileext = fileext)

	# Save the ggplot2 objects to the temporary SVG files
	suppressMessages(ggplot2::ggsave(tmp1, plot = x))
	suppressMessages(ggplot2::ggsave(tmp2, plot = y))

	# Compare the MD5 checksums of the two files
	tools::md5sum(tmp1) == tools::md5sum(tmp2)
}

logit <- function(p, percents = range.p[2] > 1, adjust) {
	range.p <- range(p, na.rm = TRUE)
	if (percents) {
		if (range.p[1] < 0 || range.p[1] > 100) {
			stop("p must be in the range 0 to 100")
		}
		p <- p / 100
		range.p <- range.p / 100
	} else if (range.p[1] < 0 || range.p[1] > 1) {
		stop("p must be in the range 0 to 1")
	}
	a <- if (missing(adjust)) {
		if (isTRUE(all.equal(range.p[1], 0)) || isTRUE(all.equal(range.p[2], 1))) {
			0.025
		} else {
			0
		}
	} else {
		adjust
	}
	if (missing(adjust) && a != 0) {
		warning(paste(
			"proportions remapped to (",
			a,
			", ",
			1 - a,
			")",
			sep = ""
		))
	}
	a <- 1 - 2 * a
	log((0.5 + a * (p - 0.5)) / (1 - (0.5 + a * (p - 0.5))))
}


# Helper function for ggplot2 version variants in visual tests
ggplot2_variant <- function() {
	if (packageVersion("ggplot2") <= "3.5.2") {
		"ggplot2-old"
	} else {
		"ggplot2-new"
	}
}

##### Helpers for plot regression tests (test-all-w2.R, test-mct.R) ----
#
# Plots are guarded in two complementary layers:
#
#   * Plot *content* - that autoplot() draws the right means, intervals and
#     letters - is checked with expect_autoplot_data() below, via
#     ggplot2::layer_data(). It depends only on data (no pixels/fonts), so it
#     runs on every platform, including CI.
#
#   * Plot *rendering* (pixel-exact vdiffr snapshots) is checked with
#     expect_local_doppelganger(). vdiffr SVGs are platform-specific, so these
#     run locally only (skipped on CI) - giving fast local confirmation that a
#     figure is unchanged, with the package released to CRAN from a local run.
#
# (In the workshop suite, test-all-w2.R, numeric and printed output is likewise
# checked at *display* precision via print.mct(), so it too is OS-stable.)

# Pixel-exact visual regression check. Skipped on CI (see above), under coverage
# and on older {grid}.
expect_local_doppelganger <- function(title, plot, variant = NULL) {
	testthat::skip_on_ci()
	testthat::skip_on_covr()
	testthat::skip_if(packageVersion("grid") < "4.2.1")
	vdiffr::expect_doppelganger(title, plot, variant = variant)
}

# Return the computed data for the single plot layer drawn by `geom` (a Geom
# class name, e.g. "GeomPoint"). Errors if the layer is absent or ambiguous.
layer_data_for <- function(plot, geom) {
	idx <- which(vapply(
		plot$layers,
		function(l) inherits(l$geom, geom),
		logical(1)
	))
	testthat::expect_length(idx, 1)
	ggplot2::layer_data(plot, idx)
}

# Content-level check for a default autoplot.mct figure (point + error bars +
# letters). Confirms the plot draws the object's own predictions - the means
# (points), interval bounds (error bars) and significance-group letters - with
# no dependence on rendered pixels, so it is safe to run on every OS and on CI.
# Compared against the object's own `predictions`, the values are internally
# consistent, so the (tiny) tolerance only guards floating-point noise; the
# cross-OS correctness of the values themselves is covered separately by the
# expect_equal() checks in each test. Not suitable for the column/HSD-bar/
# no-errorbar/no-lettering variants, which have their own structural checks.
expect_autoplot_data <- function(plot, pred, tolerance = 1e-6) {
	preds <- pred$predictions
	# autoplot() plots the back-transformed PredictedValue when a transformation
	# was applied (default, interpretable scale); otherwise predicted.value.
	yval <- if ("PredictedValue" %in% names(preds)) {
		preds$PredictedValue
	} else {
		preds$predicted.value
	}
	pts <- layer_data_for(plot, "GeomPoint")
	testthat::expect_equal(sort(pts$y), sort(yval), tolerance = tolerance)
	bars <- layer_data_for(plot, "GeomErrorbar")
	testthat::expect_equal(sort(bars$ymin), sort(preds$low), tolerance = tolerance)
	testthat::expect_equal(sort(bars$ymax), sort(preds$up), tolerance = tolerance)
	if ("groups" %in% names(preds)) {
		txt <- layer_data_for(plot, "GeomText")
		testthat::expect_equal(sort(as.character(txt$label)), sort(preds$groups))
	}
}

##### Helpers for design() tests ----

expect_design_output <- function(
	x,
	expected_names = c("design", "plot.des", "satab", "seed"),
	expected_seed = NULL,
	expect_plot = "plot.des" %in% expected_names
) {
	testthat::expect_type(x, "list")
	testthat::expect_named(x, expected_names)
	testthat::expect_true(inherits(x, "design"))
	testthat::expect_true(is.data.frame(x$design))

	testthat::expect_true(!is.null(x$satab))
	testthat::expect_true(inherits(x$satab, "satab"))

	if (!is.null(expected_seed)) {
		testthat::expect_identical(x$seed, expected_seed)
	}

	if ("seed" %in% expected_names) {
		testthat::expect_true(length(x$seed) == 1L)
		testthat::expect_true(is.numeric(x$seed))
	}

	# When plot.des isn't returned, `$` will yield NULL
	if (isTRUE(expect_plot)) {
		testthat::expect_false(is.null(x$plot.des))
		testthat::expect_true(inherits(x$plot.des, c("ggplot", "patchwork")))
	} else {
		testthat::expect_null(x$plot.des)
	}
}

expect_design_df_has_cols <- function(design_df, cols) {
	testthat::expect_true(is.data.frame(design_df))
	missing_cols <- setdiff(cols, names(design_df))
	testthat::expect_length(missing_cols, 0)
}

expect_design_df_starts_with <- function(design_df, prefix) {
	testthat::expect_true(length(names(design_df)) >= length(prefix))
	testthat::expect_identical(names(design_df)[seq_along(prefix)], prefix)
}

expect_design_df_ends_with <- function(design_df, suffix) {
	testthat::expect_true(length(names(design_df)) >= length(suffix))
	idx <- (length(names(design_df)) - length(suffix) + 1L):length(names(
		design_df
	))
	testthat::expect_identical(names(design_df)[idx], suffix)
}

expect_csv_matches_df <- function(df, csv_path, tolerance = 1e-6) {
	testthat::expect_true(file.exists(csv_path))
	csv <- utils::read.csv(csv_path, check.names = FALSE)

	testthat::expect_true(is.data.frame(df))
	testthat::expect_equal(dim(csv), dim(df))
	testthat::expect_identical(colnames(csv), colnames(df))

	for (nm in colnames(df)) {
		x <- df[[nm]]
		y <- csv[[nm]]

		if (is.numeric(x) && is.numeric(y)) {
			testthat::expect_equal(x, y, tolerance = tolerance)
		} else {
			testthat::expect_equal(as.character(x), as.character(y))
		}
	}
}

# Helpers for capturing condition output in tests

capture_messages_text <- function(expr) {
	paste(testthat::capture_messages(expr), collapse = "\n")
}

capture_warnings_text <- function(expr) {
	paste(testthat::capture_warnings(expr), collapse = "\n")
}
