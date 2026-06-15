#' Perform Multiple Comparison Tests on a statistical model
#'
#' A function for comparing and ranking predicted means with Tukey's Honest Significant Difference (HSD) Test.
#'
#' @param model.obj An `asreml`, `aov`, `lm`, `lme` ([nlme::lme()]) or `lmerMod` ([lme4::lmer()]) model object.
#' @param classify Name of predictor variable as string.
#' @param sig The significance level, numeric between 0 and 1. Default is 0.05.
#' @param int.type The type of confidence interval to calculate. One of `ci`, `tukey`, `1se`, `2se`, or `none`. Default is `ci`.
#' @param trans Transformation that was applied to the response variable. One of `log`, `sqrt`, `logit`, `power`, `inverse`, or `arcsin`. Default is `NULL`.
#' @param offset Numeric offset applied to response variable prior to transformation. Default is `NULL`. Use 0 if no offset was applied to the transformed data. See Details for more information.
#' @param power Numeric power applied to response variable with power transformation. Default is `NULL`. See Details for more information.
#' @param decimals Deprecated. Rounding is now controlled via the `decimals` argument of [print.mct()].
#' @param descending Logical (default `FALSE`). Order of the output sorted by the predicted value. If `TRUE`, largest will be first, through to smallest last.
#' @param groups Logical (default `TRUE`). If `TRUE`, the significance letter groupings will be calculated and displayed. This can get overwhelming for large numbers of comparisons, so can be turned off by setting to `FALSE`.
#' @param adjust The method used to adjust p-values for multiple comparisons. Either `"tukey"` (default, Tukey's HSD) or any method accepted by [stats::p.adjust()] (`"bonferroni"`, `"holm"`, `"hochberg"`, `"hommel"`, `"BH"` (or `"fdr"`), `"BY"`, or `"none"`). See Details.
#' @param by A character vector of column name(s) in the predictions over which to split comparisons. Comparisons are run independently within each level (or combination of levels) of the `by` variable(s); no p-values are pooled or adjusted across groups. Default `NULL`. See Details.
#' @param plot Automatically produce a plot of the output of the multiple comparison test? Default is `FALSE`. This is maintained for backwards compatibility, but the preferred method now is to use `autoplot(<multiple_comparisons output>)`. See [biometryassist::autoplot.mct()] for more details.
#' @param label_height Height of the text labels above the upper error bar on the plot. Default is 0.1 (10%) of the difference between upper and lower error bars above the top error bar.
#' @param rotation Rotate the text output as Treatments within the plot. Allows for easier reading of long treatment labels. Number between 0 and 360 (inclusive) - default 0
#' @param save Logical (default `FALSE`). Save the predicted values to a csv file?
#' @param savename A file name for the predicted values to be saved to. Default is `predicted_values`.
#' @param ... Other arguments passed internally to model-specific prediction methods.
#'
#' @importFrom multcompView multcompLetters
#' @importFrom emmeans emmeans
#' @importFrom stats model.frame predict qtukey qt terms var ptukey pt p.adjust
#' @importFrom utils packageVersion
#'
#' @details
#' ## Offset
#' Some transformations require that data has a small offset to be
#' applied, otherwise it will cause errors (for example taking a log of 0, or
#' the square root of negative values). In order to correctly reverse this
#' offset, if the `trans` argument is supplied, a value should also be supplied
#' in the `offset` argument. By default the function assumes no offset was
#' required for a transformation, implying a value of 0 for the `offset`
#' argument. If an offset value is provided, use the same value as provided in
#' the model, not the inverse. For example, if adding 0.1 to values for a log
#' transformation, add 0.1 in the `offset` argument.
#'
#' ## Power
#' The power argument allows the specification of arbitrary powers to be
#' back transformed, if they have been used to attempt to improve normality of
#' residuals.
#'
#' ## P-value adjustment (`adjust`)
#' By default (`adjust = "tukey"`) the function uses Tukey's HSD, which is exact
#' for the complete set of all pairwise comparisons. Alternatively, `adjust` may
#' be any method accepted by [stats::p.adjust()]. In that case a matrix of *raw*
#' two-sided t-test p-values is computed first and the chosen adjustment is
#' applied to the lower-triangle vector of those raw p-values, avoiding the
#' double-adjustment that would occur if Tukey-adjusted p-values were passed to
#' `p.adjust()`. Bonferroni and Holm control the family-wise error rate and are
#' valid under arbitrary dependence; BH/BY (FDR) are valid under positive
#' dependence, which pairwise comparisons satisfy. The returned
#' `$pairwise_pvalues` always holds the adjusted p-values for the chosen method
#' (the Tukey p-values when `adjust = "tukey"`). When `adjust` is not `"tukey"`,
#' `$hsd` is `NULL`, as an HSD value is only meaningful for Tukey's test.
#'
#' ## Grouped comparisons (`by`)
#' When `by` is supplied, the predictions are split into subgroups defined by
#' the level(s) of the `by` variable(s) and the comparison procedure is run
#' independently within each subgroup. Each subgroup is treated as a separate
#' family of comparisons: there is no pooling and no cross-group p-value
#' adjustment, and letter groupings restart within each subgroup. When `by` is
#' used, `$pairwise_pvalues` (and `$hsd` where applicable) are returned as named
#' lists with one element per subgroup, and `autoplot()` facets the plot by the
#' `by` variable(s) by default. `by` must leave at least one `classify` factor
#' to compare within each subgroup, so a single-factor `classify` cannot be
#' split with `by`.
#'
#' ## Confidence Intervals & Comparison Intervals
#'
#' The function provides several options for confidence intervals via the `int.type` argument:
#'
#' - **`ci` (default)**: Traditional confidence intervals for individual means. These estimate the precision of each individual mean but may not align with the multiple comparison results. Non-overlapping traditional confidence intervals do not necessarily indicate significant differences in multiple comparison tests.
#'
#' - **`tukey`**: Tukey comparison intervals that are consistent with the multiple comparison test. These intervals are wider than regular confidence intervals and are designed so that non-overlapping intervals correspond to statistically significant differences in the Tukey HSD test. This ensures visual consistency between the intervals and letter groupings.
#'
#' - **`1se`** and **`2se`**: Intervals of ±1 or ±2 standard errors around each mean.
#'
#' - **`none`**: No confidence intervals will be calculated or displayed in plots.
#'
#' By default, the function displays regular confidence intervals (`int.type = "ci"`),
#' which estimate the precision of individual treatment means. However, when
#' performing multiple comparisons, these regular confidence intervals may not
#' align with the letter groupings from Tukey's HSD test. Specifically, you may
#' observe non-overlapping confidence intervals for treatments that share the
#' same letter group (indicating no significant difference).
#'
#' This occurs because regular confidence intervals and Tukey's HSD test serve
#' different purposes:
#' - Regular confidence intervals estimate individual mean precision
#' - Tukey's HSD controls the family-wise error rate across all pairwise comparisons
#'
#' To resolve this visual inconsistency, you can use Tukey comparison intervals
#' (`int.type = "tukey"`). These intervals are specifically designed for multiple
#' comparisons and will be consistent with the letter groupings: non-overlapping
#' Tukey intervals indicate significant differences, while overlapping intervals
#' suggest no significant difference.
#'
#' The function will issue a message if it detects potential inconsistencies
#' between non-overlapping confidence intervals and letter groupings, suggesting
#' the use of Tukey intervals for clearer interpretation.
#' For multiple comparison contexts, Tukey comparison intervals are recommended as they provide visual consistency with the statistical test being performed and avoid the common confusion where traditional confidence intervals don't overlap but groups share the same significance letter.
#'
#' @returns An object of class `mct` (a list with class attributes) containing:
#'  \item{predictions}{A data frame with predicted means, standard errors,
#'    confidence interval upper and lower bounds, and significant group
#'    allocations}
#'  \item{pairwise_pvalues}{A symmetric matrix of adjusted p-values for all
#'    pairwise comparisons (Tukey's HSD by default, otherwise adjusted by the
#'    `adjust` method). When `by` is supplied, a named list of such matrices,
#'    one per subgroup}
#'  \item{hsd}{The Honest Significant Difference value(s) used in the comparisons
#'    when `adjust = "tukey"`. Either a single numeric value (if constant across
#'    comparisons) or a matrix (if it varies by comparison). `NULL` when `adjust`
#'    is not `"tukey"`}
#'  \item{sig_level}{The significance level used (default 0.05)}
#'  \item{comparison_method}{The p-value adjustment method used (the value of
#'    `adjust`)}
#'  \item{aliased}{Character vector of aliased treatment levels (only present if
#'    some predictions are aliased)}
#'
#' @references Jørgensen, E. & Pedersen, A. R. (1997). How to Obtain Those Nasty
#'  Standard Errors From Transformed Data - and Why They Should Not Be Used.
#'
#' @inheritSection get_predictions Supported model types
#'
#' @seealso [pairwise_comparisons()] for testing a chosen subset of pairwise
#'   differences as a tidy table, or [reference_comparisons()] for testing
#'   treatments against a chosen reference or control. For guidance on choosing
#'   between the two and on multiplicity adjustments, see
#'   `vignette("choosing-multiple-comparisons", "biometryassist")`.
#'
#' @examples
#' # Fit aov model
#' model <- aov(Petal.Length ~ Species, data = iris)
#'
#' # Display the ANOVA table for the model
#' anova(model)
#'
#' # Determine ranking and groups according to Tukey's Test (with Tukey intervals)
#' pred.out <- multiple_comparisons(model, classify = "Species")
#'
#' # Display the predicted values table
#' pred.out
#'
#' # Access the p-value matrix
#' pred.out$pairwise_pvalues
#'
#' # Access the HSD value
#' pred.out$hsd
#'
#' # Show the predicted values plot
#' autoplot(pred.out, label_height = 0.5)
#'
#' # Use traditional confidence intervals instead of Tukey comparison intervals
#' pred.out.ci <- multiple_comparisons(model, classify = "Species", int.type = "ci")
#' pred.out.ci
#'
#' # Plot without confidence intervals
#' pred.out.none <- multiple_comparisons(model, classify = "Species", int.type = "none")
#' autoplot(pred.out.none)
#'
#' # Use a different p-value adjustment instead of Tukey's HSD
#' multiple_comparisons(model, classify = "Species", adjust = "fdr")
#'
#' # `by`: run the comparisons independently within each level of another factor.
#' # Here a 2 x 3 factorial - compare tension levels within each wool type.
#' m_wb <- aov(breaks ~ wool * tension, data = warpbreaks)
#' mc_by <- multiple_comparisons(m_wb, classify = "wool:tension", by = "wool")
#' mc_by
#' autoplot(mc_by) # faceted by wool
#'
#' # AOV model example with transformation
#' my_iris <- iris
#' my_iris$Petal.Length <- exp(my_iris$Petal.Length) # Create exponential response
#' exp_model <- aov(Petal.Length ~ Species, data = my_iris)
#'
#' resplot(exp_model) # Residual plot shows problems
#'
#' # Fit a new model using a log transformation of the response
#' log_model <- aov(log(Petal.Length) ~ Species, data = my_iris)
#'
#' resplot(log_model) # Looks much better
#'
#' # Display the ANOVA table for the model
#' anova(log_model)
#'
#' # Back transform, because the "original" data was exponential
#' pred.out <- multiple_comparisons(log_model, classify = "Species",
#'                                    trans = "log")
#'
#' # Display the predicted values table
#' pred.out
#'
#' # Show the predicted values plot
#' autoplot(pred.out, label_height = 15)
#'
#' \dontrun{
#' # ASReml-R Example
#' library(asreml)
#'
#' # Fit ASReml-R model
#' model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
#'                     random = ~ Blocks + Blocks:Wplots,
#'                     residual = ~ units,
#'                     data = asreml::oats)
#'
#' wald(model.asr) # Nitrogen main effect significant
#'
#' #Determine ranking and groups according to Tukey's Test
#' pred.out <- multiple_comparisons(model.obj = model.asr, classify = "Nitrogen",
#'                     descending = TRUE)
#'
#' print(pred.out, decimals = 5)
#'
#' # Example using a box-cox transformation
#' set.seed(42) # See the seed for reproducibility
#' resp <- rnorm(n = 50, 5, 1)^3
#' trt <- as.factor(sample(rep(LETTERS[1:10], 5), 50))
#' block <- as.factor(rep(1:5, each = 10))
#' ex_data <- data.frame(resp, trt, block)
#'
#' # Change one treatment random values to get significant difference
#' ex_data$resp[ex_data$trt=="A"] <- rnorm(n = 5, 7, 1)^3
#'
#' model.asr <- asreml(resp ~ trt,
#'                     random = ~ block,
#'                     residual = ~ units,
#'                     data = ex_data)
#'
#' resplot(model.asr)
#'
#' # lambda = 1/3 from MASS::boxcox()
#' model.asr <- asreml(resp^(1/3) ~ trt,
#'                     random = ~ block,
#'                     residual = ~ units,
#'                     data = ex_data)
#'
#' resplot(model.asr) # Look much better
#'
#' pred.out <- multiple_comparisons(model.obj = model.asr,
#'                                  classify = "trt",
#'                                  trans = "power", power = (1/3))
#'
#' pred.out
#' autoplot(pred.out, label_height = 0.5)
#' }
#'
#' @export
#'
multiple_comparisons <- function(
	model.obj,
	classify,
	sig = 0.05,
	int.type = "ci",
	trans = NULL,
	offset = NULL,
	power = NULL,
	decimals = 2,
	descending = FALSE,
	groups = TRUE,
	adjust = "tukey",
	by = NULL,
	plot = FALSE,
	label_height = 0.1,
	rotation = 0,
	save = FALSE,
	savename = "predicted_values",
	...
) {
	# Parameters removed in 1.5.0: give a clear error with migration guidance
	.removed_params <- list(
		pred = "`pred` was removed in biometryassist 1.5.0. Use `classify` instead.",
		order = "`order` was removed in biometryassist 1.5.0. Use `descending` instead.",
		pred.obj = paste0(
			"`pred.obj` was removed in biometryassist 1.5.0. ",
			"Predictions are now performed internally in the function."
		)
	)
	.early_dots <- list(...)
	for (.p in names(.removed_params)) {
		if (.p %in% names(.early_dots)) {
			stop(.removed_params[[.p]], call. = FALSE)
		}
	}

	# Handle deprecated parameters
	handle_deprecated_param(
		"decimals",
		NULL,
		"Rounding is now controlled via the `decimals` argument of `print.mct()`."
	)
	handle_deprecated_param(
		"plot",
		NULL,
		"Use `autoplot(<multiple_comparisons output>)` instead."
	)
	handle_deprecated_param(
		"label_height",
		NULL,
		"Pass `label_height` to `autoplot()` instead."
	)
	handle_deprecated_param(
		"rotation",
		NULL,
		"Pass `rotation` to `autoplot()` instead."
	)
	handle_deprecated_param(
		"save",
		NULL,
		"Use `write.csv(result$predictions, \"filename.csv\")` instead."
	)
	handle_deprecated_param(
		"savename",
		NULL,
		"Use `write.csv(result$predictions, \"filename.csv\")` instead."
	)

	vars <- validate_inputs(sig, classify, model.obj, trans)

	# Process dots
	rlang::check_dots_used()
	args = list(...)

	# Check for alias 'letters' instead of 'groups'
	if ("letters" %in% names(args)) {
		if (!missing(groups)) {
			warning("Both 'groups' and 'letters' provided. Using 'groups'.")
		} else {
			groups <- args$letters
		}
	}

	# Validate p-value adjustment method
	valid_adjust <- c(
		"tukey",
		"holm",
		"hochberg",
		"hommel",
		"bonferroni",
		"BH",
		"BY",
		"fdr",
		"none"
	)
	if (length(adjust) != 1 || !adjust %in% valid_adjust) {
		stop(
			"Invalid `adjust` method. Must be one of: ",
			paste(valid_adjust, collapse = ", "),
			".",
			call. = FALSE
		)
	}

	# Get model-specific predictions and SED
	result <- get_predictions(model.obj, classify, ...)

	pp <- result$predictions
	sed <- result$sed
	ndf <- result$df
	ylab <- result$ylab
	aliased <- result$aliased_names

	# Process treatment names
	pp <- process_treatment_names(pp, vars)

	# Validate `by` columns
	if (!is.null(by)) {
		if (!all(by %in% names(pp))) {
			missing_by <- by[by %!in% names(pp)]
			stop(
				"The `by` variable(s) ",
				paste(missing_by, collapse = ", "),
				" are not present in the predictions. Available columns are: ",
				paste(names(pp), collapse = ", "),
				".",
				call. = FALSE
			)
		}
		# At least one `classify` factor must remain to compare within each group.
		# This rules out a single-factor `classify`, or a `by` that consumes all
		# of the `classify` variable(s).
		if (length(setdiff(vars, by)) < 1) {
			stop(
				"`by` cannot include all of the `classify` variable(s); at least ",
				"one factor must remain to compare within each group. A `classify` ",
				"with a single factor cannot be split using `by`.",
				call. = FALSE
			)
		}
	}

	# Run the comparison procedure for a single (sub)group of predictions.
	# Returns the (optionally letter-grouped) predictions plus the p-value
	# matrix (Tukey-adjusted or `adjust`-adjusted) and the Tukey critical value
	# (NULL for other methods).
	run_comparisons <- function(pp_g, sed_g, ndf_g) {
		if (adjust == "tukey") {
			# Tukey-adjusted p-values via the studentized range distribution.
			# calculate_pvalue_matrix() drops dimnames when ndf is a matrix
			# (aovlist/lmer), so restore them from the predictions order.
			tukey_matrix <- calculate_pvalue_matrix(pp_g, sed_g, ndf_g)
			dimnames(tukey_matrix) <- list(pp_g$Names, pp_g$Names)
			crit_val_g <- 1 /
				sqrt(2) *
				stats::qtukey((1 - sig), nrow(pp_g), ndf_g) *
				sed_g
			diff_res <- get_diffs_from_pvalues(tukey_matrix, sig, adjust)
		} else {
			# Raw two-sided t-test p-values are adjusted via stats::p.adjust().
			# Computing raw p-values first avoids double-adjustment.
			raw_matrix <- calculate_raw_pvalue_matrix(pp_g, sed_g, ndf_g)
			crit_val_g <- NULL
			diff_res <- get_diffs_from_pvalues(raw_matrix, sig, adjust)
		}

		if (groups) {
			pp_g <- add_letter_groups(pp_g, diff_res$diffs, descending)
		}

		# Confidence/comparison intervals are computed per (sub)group so that
		# `int.type = "tukey"` uses this group's mean count and degrees of freedom.
		# Computing them once on the recombined table would use the total mean
		# count, making the intervals inconsistent with the per-group letter
		# groupings. For the no-`by` case this group is the whole table.
		pp_g <- add_confidence_intervals(pp_g, int.type, sig, ndf_g)

		# Detect (but do not yet emit) any CI/letter inconsistency within this
		# group; the caller emits a single note if any group is inconsistent.
		ci_inconsistent <- FALSE
		if (groups && tolower(int.type) == "ci" && adjust == "tukey") {
			ci_inconsistent <- check_ci_consistency(pp_g)
		}

		list(
			pp = pp_g,
			pval_matrix = diff_res$adjusted_matrix,
			crit_val = crit_val_g,
			ci_inconsistent = ci_inconsistent
		)
	}

	if (is.null(by)) {
		res <- run_comparisons(pp, sed, ndf)
		pp <- res$pp
		pval_matrix <- res$pval_matrix
		crit_val <- res$crit_val
		ci_inconsistent <- res$ci_inconsistent
	} else {
		by_vals <- interaction(pp[, by, drop = FALSE], drop = TRUE)
		groups_list <- split(seq_len(nrow(pp)), by_vals)

		res_list <- lapply(groups_list, function(idx) {
			pp_g <- pp[idx, , drop = FALSE]
			sed_g <- sed[idx, idx, drop = FALSE]
			ndf_g <- if (is.matrix(ndf)) ndf[idx, idx, drop = FALSE] else ndf
			run_comparisons(pp_g, sed_g, ndf_g)
		})

		pp <- do.call(rbind, lapply(res_list, function(r) r$pp))
		rownames(pp) <- NULL
		pval_matrix <- lapply(res_list, function(r) r$pval_matrix)
		crit_val <- lapply(res_list, function(r) r$crit_val)
		ci_inconsistent <- any(vapply(
			res_list,
			function(r) r$ci_inconsistent,
			logical(1)
		))
	}

	# Apply transformations if requested
	if (!is.null(trans)) {
		pp <- apply_transformation(pp, trans, offset, power)
	} else {
		pp$low <- pp$predicted.value - pp$ci
		pp$up <- pp$predicted.value + pp$ci
	}

	# Order results and format output
	pp <- format_output(pp, descending, vars, by)

	# Save if requested (full precision values)
	if (save) {
		utils::write.csv(pp, file = paste0(savename, ".csv"), row.names = FALSE)
	}

	# Emit a single CI/letter inconsistency note if any (sub)group triggered it.
	# Detection happens per (sub)group inside run_comparisons() so that grouped
	# comparisons (`by`) are covered too.
	if (isTRUE(ci_inconsistent)) {
		message(
			"Note: Some treatments sharing the same letter group have non-overlapping confidence intervals.\n",
			"This is expected behavior as regular confidence intervals estimate individual mean precision,\n",
			"while Tukey's HSD controls family-wise error rates. For visual consistency with letter groups,\n",
			"consider using 'int.type = \"tukey\"' to display Tukey comparison intervals."
		)
	}

	# Prepare HSD value(s) for output. Only meaningful for Tukey's HSD.
	summarise_hsd <- function(cv) {
		if (isTRUE(stats::var(as.vector(cv), na.rm = TRUE) < 1e-10)) {
			cv[1, 2]
		} else {
			cv
		}
	}
	if (adjust != "tukey") {
		hsd_output <- NULL
	} else if (is.null(by)) {
		hsd_output <- summarise_hsd(crit_val)
	} else {
		hsd_output <- lapply(crit_val, summarise_hsd)
	}

	# Create output list. The first four elements (predictions, pairwise_pvalues,
	# hsd, sig_level) keep their historical positions for backwards-compatible
	# positional access; new elements are appended after them. `pairwise_pvalues`
	# holds the adjusted p-values (Tukey's by default, otherwise the `adjust`
	# method).
	output <- list(
		predictions = pp,
		pairwise_pvalues = pval_matrix,
		hsd = hsd_output,
		sig_level = sig,
		comparison_method = adjust
	)

	# Add aliased treatments if present
	if (!is.null(aliased)) {
		output$aliased <- as.character(aliased)
	}

	# Strip transformation from ylab if trans is provided
	ylab <- strip_transformation_from_label(ylab, trans)

	# Add attributes for backward compatibility
	attr(output, "ylab") <- ylab
	attr(output, "HSD") <- hsd_output # Keep for backward compatibility
	if (!is.null(aliased)) {
		attr(output, "aliased") <- as.character(aliased)
	}
	# Record the `by` variable(s) so autoplot() can use them for faceting
	if (!is.null(by)) {
		attr(output, "by") <- by
	}
	# Record the back-transformation so autoplot() can build an exact
	# back-transformed secondary axis when plotting on the model scale.
	if (!is.null(trans)) {
		attr(output, "trans") <- trans
		attr(output, "offset") <- if (is.null(offset)) 0 else offset
		attr(output, "power") <- power
	}

	# Add class
	class(output) <- c("mct", "list")

	# Plot if requested
	if (plot) {
		print(autoplot(output))
	}

	return(output)
}


#' Print output of multiple_comparisons
#'
#' @param x An mct object to print to the console.
#' @param decimals Number of decimal places to display. Default is 2.
#' @param ... Other arguments passed to print.data.frame
#'
#' @returns The original object invisibly.
#' @seealso [multiple_comparisons()]
#' @method print mct
#' @export
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#' output <- multiple_comparisons(dat.aov, classify = "Species")
#' print(output)
#' print(output, decimals = 4)
print.mct <- function(x, decimals = 2, ...) {
	stopifnot(inherits(x, "mct"))

	method <- if (is.null(x$comparison_method)) "tukey" else x$comparison_method

	if (method == "tukey") {
		cat("Multiple Comparisons of Means: Tukey's HSD Test\n")
		cat("Significance level:", x$sig_level, "\n")
		cat(
			"HSD value:",
			if (length(x$hsd) == 1) x$hsd else "varies by comparison (see $hsd)\n",
			"\n"
		)
	} else {
		cat(
			"Multiple Comparisons of Means: p-value adjustment =",
			method,
			"\n"
		)
		cat("Significance level:", x$sig_level, "\n")
	}
	cat("\nPredicted values:\n")
	pp <- x$predictions
	numeric_cols <- vapply(pp, is.numeric, logical(1))
	pp[numeric_cols] <- lapply(pp[numeric_cols], round, decimals)
	print.data.frame(pp, ...)

	note <- aliased_note(x$aliased)
	if (!is.null(note)) {
		cat("\n", note, "\n", sep = "")
	}

	invisible(x)
}


#' Calculate matrix of p-values for all pairwise comparisons
#'
#' @param pp Data frame with predicted values and Names column
#' @param sed Standard error of differences (matrix or scalar)
#' @param ndf Degrees of freedom
#'
#' @return Symmetric matrix of p-values with treatment names as row/column names
#' @noRd
calculate_pvalue_matrix <- function(pp, sed, ndf) {
	n <- nrow(pp)
	treatment_names <- pp$Names

	# Initialize p-value matrix
	pval_matrix <- matrix(
		1,
		nrow = n,
		ncol = n,
		dimnames = list(treatment_names, treatment_names)
	)

	if (n <= 1) {
		return(pval_matrix)
	}

	# Vectorized pairwise differences for the upper triangle
	pair_idx <- which(upper.tri(pval_matrix), arr.ind = TRUE)
	i <- pair_idx[, 1]
	j <- pair_idx[, 2]

	diff <- abs(pp$predicted.value[i] - pp$predicted.value[j])

	# Get the appropriate SED for each comparison.
	# `sed` may be a scalar, a base matrix, or a Matrix::* matrix.
	sed_ij <- if (!is.null(dim(sed))) {
		as.numeric(sed[cbind(i, j)])
	} else {
		rep_len(sed, length(diff))
	}

	# Calculate the studentized range statistic and p-values in one call
	q_stat <- as.numeric(diff / sed_ij) * sqrt(2)

	# If ndf is a matrix, convert q_stat to a matrix before calculating p-value matrix
	if (is.matrix(ndf) == TRUE) {
		q_stat_matrix <- matrix(0, nrow = n, ncol = n)
		q_stat_matrix[cbind(i, j)] <- q_stat
		q_stat_matrix[cbind(j, i)] <- q_stat
		pval_matrix <- stats::ptukey(
			q_stat_matrix,
			nmeans = n,
			df = ndf,
			lower.tail = FALSE
		)
	} else {
		pvals <- stats::ptukey(q_stat, nmeans = n, df = ndf, lower.tail = FALSE)

		# if pvals is not a matrix, then convert it to a matrix
		if (is.matrix(pvals) == FALSE) {
			pval_matrix[cbind(i, j)] <- pvals
			pval_matrix[cbind(j, i)] <- pvals
		}
	}

	return(pval_matrix)
}


#' Calculate matrix of raw (unadjusted) pairwise p-values
#'
#' Computes a symmetric matrix of two-sided t-test p-values for all pairwise
#' comparisons. These are the *unadjusted* p-values; any multiple-comparison
#' adjustment is applied later. Computing raw p-values first avoids the
#' double-adjustment that would occur if Tukey-adjusted p-values were passed to
#' [stats::p.adjust()].
#'
#' @param pp Data frame with predicted values and a `Names` column
#' @param sed Standard error of differences (matrix or scalar)
#' @param ndf Degrees of freedom (scalar or matrix)
#'
#' @return Symmetric matrix of raw p-values with treatment names as row/column
#'   names (diagonal set to 1)
#' @noRd
calculate_raw_pvalue_matrix <- function(pp, sed, ndf) {
	n <- nrow(pp)
	treatment_names <- pp$Names

	pval_matrix <- matrix(
		1,
		nrow = n,
		ncol = n,
		dimnames = list(treatment_names, treatment_names)
	)

	if (n <= 1) {
		return(pval_matrix)
	}

	pair_idx <- which(upper.tri(pval_matrix), arr.ind = TRUE)
	i <- pair_idx[, 1]
	j <- pair_idx[, 2]

	diff <- abs(pp$predicted.value[i] - pp$predicted.value[j])

	sed_ij <- if (!is.null(dim(sed))) {
		as.numeric(sed[cbind(i, j)])
	} else {
		rep_len(sed, length(diff))
	}

	ndf_ij <- if (is.matrix(ndf)) {
		as.numeric(ndf[cbind(i, j)])
	} else {
		rep_len(ndf, length(diff))
	}

	t_stat <- diff / sed_ij
	p_raw <- 2 * stats::pt(-abs(t_stat), df = ndf_ij)

	pval_matrix[cbind(i, j)] <- p_raw
	pval_matrix[cbind(j, i)] <- p_raw

	return(pval_matrix)
}


#' Determine significant differences from a p-value matrix
#'
#' Replaces `get_diffs()` as the source of the `diffs` named logical vector
#' consumed by [multcompView::multcompLetters3()].
#'
#' For `adjust = "tukey"`, `pval_matrix` is expected to already contain
#' Tukey-adjusted p-values (no further adjustment is applied). For any other
#' method, `pval_matrix` is expected to contain raw two-sided t-test p-values,
#' and the requested [stats::p.adjust()] method is applied to the lower-triangle
#' vector of p-values.
#'
#' @param pval_matrix Symmetric p-value matrix with treatment names as row/col
#'   names (Tukey-adjusted when `adjust == "tukey"`, otherwise raw)
#' @param sig Significance level
#' @param adjust Adjustment method (`"tukey"` or a [stats::p.adjust()] method)
#'
#' @return A list with `diffs` (named logical vector, `TRUE` = significantly
#'   different), `adjusted_pvalues` (named numeric vector for the lower
#'   triangle), `adjusted_matrix` (symmetric adjusted p-value matrix), and
#'   `pair_names`.
#' @noRd
get_diffs_from_pvalues <- function(pval_matrix, sig, adjust) {
	nm <- rownames(pval_matrix)
	n <- nrow(pval_matrix)

	lower <- lower.tri(pval_matrix)
	m <- outer(nm, nm, paste, sep = "-")
	pair_names <- m[lower]
	p_low <- as.numeric(pval_matrix[lower])

	# Apply adjustment. For Tukey, p_low is already adjusted via the
	# studentized range distribution, so no further adjustment is applied.
	p_adj <- p_low
	if (adjust != "tukey") {
		p_adj <- stats::p.adjust(p_low, method = adjust)
	}

	names(p_adj) <- pair_names

	# diffs: TRUE = significantly different.
	diffs <- p_adj < sig
	diffs[is.na(diffs)] <- FALSE
	names(diffs) <- pair_names

	# Reconstruct a symmetric adjusted p-value matrix for output
	adj_matrix <- matrix(
		NA_real_,
		nrow = n,
		ncol = n,
		dimnames = list(nm, nm)
	)
	diag(adj_matrix) <- 1
	adj_matrix[lower] <- p_adj
	adj_matrix[upper.tri(adj_matrix)] <- t(adj_matrix)[upper.tri(adj_matrix)]

	list(
		diffs = diffs,
		adjusted_pvalues = p_adj,
		adjusted_matrix = adj_matrix,
		pair_names = pair_names
	)
}


#' @noRd
add_letter_groups <- function(pp, diffs, descending) {
	ll <- multcompView::multcompLetters3(
		"Names",
		"predicted.value",
		diffs,
		pp,
		reversed = !descending
	)

	rr <- data.frame(groups = ll$Letters)
	rr$Names <- row.names(rr)

	pp <- merge(pp, rr)
	return(pp)
}


#' @noRd
format_output <- function(pp, descending, vars, by = NULL) {
	# Order by predicted value. When `by` is supplied, keep subgroups together
	# (ascending by the `by` variable(s)) and order by predicted value within.
	if (is.null(by)) {
		pp <- pp[base::order(pp$predicted.value, decreasing = descending), ]
	} else {
		pv <- if (descending) -pp$predicted.value else pp$predicted.value
		order_args <- c(lapply(by, function(b) pp[[b]]), list(pv))
		pp <- pp[do.call(base::order, order_args), ]
	}

	# Remove Names column
	pp$Names <- NULL

	# Extract treatment variable names
	trtindex <- max(unlist(lapply(paste0("^", vars, "$"), grep, x = names(pp))))
	trtnam <- names(pp)[1:trtindex]

	# Exclude reserved column names
	reserved_cols <- c(
		"predicted.value",
		"std.error",
		"Df",
		"groups",
		"PredictedValue",
		"ApproxSE",
		"ci",
		"low",
		"up"
	)
	trtnam <- trtnam[trtnam %!in% reserved_cols]

	# Convert treatment columns to factors with ordered levels
	for (i in seq_along(trtnam)) {
		pp[[trtnam[i]]] <- factor(
			pp[[trtnam[i]]],
			levels = unique(pp[[trtnam[i]]])
		)
	}

	# Remove row names
	rownames(pp) <- NULL
	return(pp)
}


#' @noRd
strip_transformation_from_label <- function(ylab, trans) {
	if (is.null(trans)) {
		return(ylab)
	}

	# Convert to character
	ylab_char <- if (is.language(ylab)) deparse(ylab) else as.character(ylab)

	# Simple pattern matching for common transformations
	stripped <- switch(
		trans,
		"log" = sub("^log(10)?\\((.+)\\)$", "\\2", ylab_char),
		"sqrt" = sub("^sqrt\\((.+)\\)$", "\\1", ylab_char),
		"logit" = sub("^logit\\((.+)\\)$", "\\1", ylab_char),
		"arcsin" = sub("^a?r?c?sin\\(sqrt\\((.+)\\)\\)$", "\\1", ylab_char),
		"inverse" = sub("^\\(?1/([^)]+)\\)?$", "\\1", ylab_char),
		"power" = sub("^\\(?(.+?)\\)?\\^.+$", "\\1", ylab_char),
		ylab_char # default: return as-is
	)

	return(stripped)
}


#' Detect CI/letter-group inconsistencies within a single (sub)group
#'
#' Returns `TRUE` if any pair of treatments with non-overlapping confidence
#' intervals nonetheless shares a letter group (the common source of confusion
#' when `int.type = "ci"` is used with Tukey's HSD). Detection only; the caller
#' is responsible for emitting the user-facing note (once, even across `by`
#' subgroups).
#'
#' @param pp Predictions for one (sub)group, with `predicted.value`, `ci` and
#'   `groups` columns.
#' @return A single logical.
#' @noRd
check_ci_consistency <- function(pp) {
	# Pre-extract and validate data once
	n <- nrow(pp)
	low <- pp$predicted.value - pp$ci
	up <- pp$predicted.value + pp$ci
	groups <- as.character(pp$groups)

	# Pre-process group letters efficiently
	group_letters <- vector("list", length(groups))
	for (i in seq_along(groups)) {
		group_letters[[i]] <- unique(strsplit(groups[i], "")[[1]])
	}

	# Vectorized overlap detection using outer operations
	# Two intervals [a,b] and [c,d] overlap if max(a,c) <= min(b,d)
	low_matrix <- matrix(low, nrow = n, ncol = n, byrow = TRUE)
	up_matrix <- matrix(up, nrow = n, ncol = n, byrow = FALSE)

	# Only compute upper triangle to avoid redundant comparisons
	upper_tri <- upper.tri(matrix(TRUE, n, n))

	# Vectorized overlap check
	max_lows <- pmax(low_matrix, t(low_matrix))
	min_ups <- pmin(up_matrix, t(up_matrix))
	overlaps <- max_lows <= min_ups

	# Find non-overlapping pairs in upper triangle only
	non_overlapping <- upper_tri & !overlaps

	if (!any(non_overlapping)) {
		return(FALSE)
	}

	# Get indices efficiently
	indices <- which(non_overlapping, arr.ind = TRUE)

	# Check for shared letters only among non-overlapping pairs
	for (k in seq_len(nrow(indices))) {
		i <- indices[k, 1]
		j <- indices[k, 2]

		if (
			groups[i] == groups[j] |
				length(intersect(group_letters[[i]], group_letters[[j]])) > 0
		) {
			return(TRUE)
		}
	}

	FALSE
}


#' @noRd
process_treatment_names <- function(pp, vars) {
	# Create Names column (interaction factors joined with "_")
	pp$Names <- make_treatment_labels(pp, vars, sep = "_")

	# Check and replace dashes in treatment names
	if (any(grepl("-", pp$Names) | any(grepl("-", pp[, 1])))) {
		levs <- unique(c(
			grep("-", pp[, 1], value = TRUE),
			grep("-", pp$Names, value = TRUE)
		))
		if (length(levs) > 1) {
			warning(
				"The treatment levels ",
				paste(levs, collapse = ", "),
				" contained '-', which has been replaced in the final output with '_'"
			)
		} else {
			warning(
				"The treatment level ",
				levs,
				" contained '-', which has been replaced in the final output with '_'"
			)
		}
		pp[, 1] <- gsub(pattern = "-", replacement = "_", pp[, 1])
		pp$Names <- gsub(pattern = "-", replacement = "_", pp$Names)
	}

	return(pp)
}


#' @noRd
get_diffs <- function(pp, sed, ndf, sig) {
	# Calculate the critical value
	crit_val <- 1 / sqrt(2) * stats::qtukey((1 - sig), nrow(pp), ndf) * sed

	# Determine pairs that are significantly different
	diffs <- abs(outer(pp$predicted.value, pp$predicted.value, "-")) > crit_val
	diffs <- diffs[lower.tri(diffs)]

	# Create a vector of treatment comparison names
	m <- outer(pp$Names, pp$Names, paste, sep = "-")
	m <- m[lower.tri(m)]

	names(diffs) <- m

	# Return both the critical value and the differences
	return(list(crit_val = crit_val, diffs = diffs))
}


#' @noRd
add_confidence_intervals <- function(pp, int.type, sig, ndf) {
	# Calculate confidence interval width
	# If denominator df is a type matrix, use the max value (TEMPORARY SOLUTION!)
	if (is.matrix(ndf) == TRUE) {
		ndf <- max(ndf, na.rm = TRUE)
	}
	pp$ci <- switch(
		tolower(int.type),
		"ci" = stats::qt(p = sig / 2, ndf, lower.tail = FALSE) * pp$std.error,
		"tukey" = stats::qtukey(p = 1 - sig, nmeans = nrow(pp), df = ndf) /
			sqrt(2) *
			pp$std.error,
		"1se" = pp$std.error,
		"2se" = 2 * pp$std.error,
		"none" = 0,
		stop("Invalid int.type. Use 'ci', 'tukey', '1se', '2se', or 'none'.")
	)

	return(pp)
}


#' Back-transform values from the model (transformed) scale to the original scale
#'
#' Single source of truth for the inverse of each supported transformation. Used
#' both to compute the `PredictedValue` column in `apply_transformation()` and to
#' build the back-transformed secondary axis in [autoplot.mct()], so the two can
#' never drift apart.
#'
#' @param x Numeric vector on the model (transformed) scale.
#' @param trans One of `"sqrt"`, `"log"`, `"logit"`, `"power"`, `"inverse"`, `"arcsin"`.
#' @param offset Additive offset used on the original scale (applied for `sqrt`,
#'   `log` and `power`). `NULL` is treated as `0`.
#' @param power The power used for `trans = "power"`.
#' @noRd
back_transform <- function(x, trans, offset = 0, power = NULL) {
	if (is.null(offset)) {
		offset <- 0
	}
	switch(
		trans,
		"sqrt" = x^2 - offset,
		"log" = exp(x) - offset,
		"logit" = exp(x) / (1 + exp(x)),
		"power" = x^(1 / power) - offset,
		"inverse" = 1 / x,
		"arcsin" = sin(x)^2,
		stop(
			"Invalid trans value. Must be one of: 'sqrt', 'log', 'logit', 'power', 'inverse', 'arcsin'.",
			call. = FALSE
		)
	)
}


apply_transformation <- function(pp, trans, offset, power) {
	# Set default offset if not provided
	if (is.null(offset)) {
		warning(
			"Offset value assumed to be 0. Change with `offset` argument.",
			call. = FALSE
		)
		offset <- 0
	}

	# Apply appropriate transformation
	if (trans == "sqrt") {
		# From paper: g(x) = sqrt(x), g^-1(y) = y^2, g'(x) = 1/(2*sqrt(x))
		# Back-transformed value: X~ = Y^2 - offset
		# Approx SE: ~X = 2*sqrt(X~ + offset)*Y_se
		pp$PredictedValue <- back_transform(pp$predicted.value, "sqrt", offset)

		# Bounds check: predicted values should be non-negative after transformation
		if (any(pp$PredictedValue < 0, na.rm = TRUE)) {
			warning(
				"Square root back-transformation produced negative values. Check offset parameter.",
				call. = FALSE
			)
		}

		# SE uses value on original scale (before offset removal) per paper formula
		pp$ApproxSE <- 2 * abs(pp$std.error) * abs(pp$predicted.value)

		pp$low <- (pp$predicted.value - pp$ci)^2 - offset
		pp$up <- (pp$predicted.value + pp$ci)^2 - offset
	} else if (trans == "log") {
		# From paper: g(x) = ln(x), g^-1(y) = exp(y), g'(x) = 1/x
		# Back-transformed value: X~ = exp(Y) - offset
		# Approx SE: ~X = X~*Y_se (where X~ is before offset removal)
		x_tilde <- exp(pp$predicted.value)
		pp$PredictedValue <- back_transform(pp$predicted.value, "log", offset)

		# Bounds check: values should be positive after offset removal
		if (any(pp$PredictedValue <= 0, na.rm = TRUE)) {
			warning(
				"Log back-transformation produced non-positive values. Check offset parameter.",
				call. = FALSE
			)
		}

		# SE uses back-transformed value before offset removal
		pp$ApproxSE <- abs(pp$std.error) * x_tilde

		pp$low <- exp(pp$predicted.value - pp$ci) - offset
		pp$up <- exp(pp$predicted.value + pp$ci) - offset
	} else if (trans == "logit") {
		# From paper: g(x) = ln(x/(1-x)), g^-1(y) = exp(y)/(1+exp(y)), g'(x) = 1/(x(1-x))
		# Back-transformed value: X~ = exp(Y)/(1+exp(Y))
		# Approx SE: ~X = X~*(1-X~)*Y_se
		pp$PredictedValue <- back_transform(pp$predicted.value, "logit")

		# Bounds check: values should be in (0, 1)
		if (any(pp$PredictedValue <= 0 | pp$PredictedValue >= 1, na.rm = TRUE)) {
			warning(
				"Logit back-transformation produced values outside (0,1). This may indicate numerical issues.",
				call. = FALSE
			)
		}

		pp$ApproxSE <- pp$PredictedValue *
			(1 - pp$PredictedValue) *
			abs(pp$std.error)

		ll <- pp$predicted.value - pp$ci
		pp$low <- exp(ll) / (1 + exp(ll))
		uu <- pp$predicted.value + pp$ci
		pp$up <- exp(uu) / (1 + exp(uu))
	} else if (trans == "power") {
		# From paper: g(x) = x^a, g^-1(y) = y^(1/a), g'(x) = a*x^(a-1)
		# Back-transformed value: X~ = Y^(1/a) - offset
		# Approx SE: ~X = Y_se / |a*X~^(a-1)| (where X~ is before offset removal)
		if (is.null(power) || !is.numeric(power) || power == 0) {
			stop(
				"Power transformation requires a non-zero numeric 'power' argument.",
				call. = FALSE
			)
		}

		x_tilde <- (pp$predicted.value)^(1 / power)
		pp$PredictedValue <- back_transform(
			pp$predicted.value,
			"power",
			offset,
			power
		)

		# Bounds check depends on power sign and offset
		if (
			power < 0 &&
				any(abs(pp$PredictedValue) < .Machine$double.eps, na.rm = TRUE)
		) {
			warning(
				"Power back-transformation with negative power produced values near zero.",
				call. = FALSE
			)
		}

		# SE uses back-transformed value before offset removal (x_tilde not PredictedValue)
		pp$ApproxSE <- abs(pp$std.error) / abs(power * x_tilde^(power - 1))

		pp$low <- (pp$predicted.value - pp$ci)^(1 / power) - offset
		pp$up <- (pp$predicted.value + pp$ci)^(1 / power) - offset
	} else if (trans == "inverse") {
		# From paper: g(x) = 1/x, g^-1(y) = 1/y, g'(x) = -1/x^2
		# Back-transformed value: X~ = 1/Y
		# Approx SE: ~X = X~^2*Y_se (using absolute value of g'(x))

		# Bounds check: avoid division by zero
		if (any(abs(pp$predicted.value) < .Machine$double.eps, na.rm = TRUE)) {
			warning(
				"Inverse transformation: predicted values very close to zero detected.",
				call. = FALSE
			)
		}

		pp$PredictedValue <- back_transform(pp$predicted.value, "inverse")
		pp$ApproxSE <- abs(pp$std.error) * pp$PredictedValue^2

		# Check for sign changes across confidence interval
		lower_bound <- pp$predicted.value - pp$ci
		upper_bound <- pp$predicted.value + pp$ci

		if (any(sign(lower_bound) != sign(upper_bound), na.rm = TRUE)) {
			warning(
				"Inverse transformation: confidence interval crosses zero. Results may be unreliable.",
				call. = FALSE
			)
		}

		# Inverse function is monotone decreasing, so bounds swap
		# Use pmin/pmax to ensure pp$low < pp$up regardless of sign
		inv_lower <- 1 / (pp$predicted.value + pp$ci)
		inv_upper <- 1 / (pp$predicted.value - pp$ci)

		pp$low <- pmin(inv_lower, inv_upper)
		pp$up <- pmax(inv_lower, inv_upper)
	} else if (trans == "arcsin") {
		# From paper: g(x) = sin^-1(sqrt(x)), g^-1(y) = sin(y)^2,
		# g'(x) = 1/(2*sqrt(x)*cos(sin^-1(sqrt(x))))
		# Back-transformed value: X~ = sin(Y)^2
		# Approx SE: ~X = 2*sqrt(X~)*cos(sin^-1(sqrt(X~)))*Y_se

		# Bounds check: predicted.value should be in valid arcsin range
		if (
			any(
				pp$predicted.value < -pi / 2 | pp$predicted.value > pi / 2,
				na.rm = TRUE
			)
		) {
			warning(
				"Arcsin transformation: some predicted values outside [-pi/2, pi/2].",
				call. = FALSE
			)
		}

		pp$PredictedValue <- back_transform(pp$predicted.value, "arcsin")

		# Bounds check: values should be in [0, 1]
		if (any(pp$PredictedValue < 0 | pp$PredictedValue > 1, na.rm = TRUE)) {
			warning(
				"Arcsin back-transformation produced values outside [0,1]. This may indicate numerical issues.",
				call. = FALSE
			)
		}

		# Simplify: cos(asin(sqrt(x))) = sqrt(1 - x) for x in [0,1]
		pp$ApproxSE <- 2 *
			abs(pp$std.error) *
			sqrt(pp$PredictedValue * (1 - pp$PredictedValue))

		pp$low <- sin(pp$predicted.value - pp$ci)^2
		pp$up <- sin(pp$predicted.value + pp$ci)^2
	} else {
		stop(
			"Invalid trans value. Must be one of: 'sqrt', 'log', 'logit', 'power', 'inverse', 'arcsin'.",
			call. = FALSE
		)
	}

	return(pp)
}


#' @noRd
add_attributes <- function(pp, ylab, crit_val, aliased_names, trans = NULL) {
	# Strip transformation from ylab if trans is provided
	ylab <- strip_transformation_from_label(ylab, trans)

	attr(pp, "ylab") <- ylab

	# Add class
	class(pp) <- c("mct", class(pp))

	# Add aliased treatments as attribute
	if (!is.null(aliased_names)) {
		attr(pp, 'aliased') <- as.character(aliased_names)
	}

	# Add critical value as attribute
	if (stats::var(as.vector(crit_val), na.rm = TRUE) < 1e-10) {
		attr(pp, 'HSD') <- crit_val[1, 2]
	} else {
		attr(pp, 'HSD') <- crit_val
	}

	return(pp)
}
