#' Compare predicted means against a single reference (control) level
#'
#' Compare every level of a treatment factor against one chosen **reference**
#' (control) level, returning a tidy, means-centric table: the predicted mean of
#' each level, the reference mean, their difference, and a multiplicity-adjusted
#' p-value for the difference. This is the honest representation of the common
#' "how does each treatment compare to the control?" question (a Dunnett-style
#' analysis), where significance attaches cleanly to each treatment because every
#' comparison shares the same reference.
#'
#' Unlike [multiple_comparisons()] (all pairs, means + letters) and
#' [pairwise_comparisons()] (selected differences), `reference_comparisons()`
#' compares each level to a single control and presents the result around the
#' means. It works for every model supported by [multiple_comparisons()].
#'
#' @param model.obj An ASReml-R, `aov`, `lm`, `lme` ([nlme::lme()]) or `lmerMod`
#'   ([lme4::lmer()]) model object.
#' @param classify Name of the predictor variable(s) to compare, as a string.
#'   Interactions are specified with `:` (e.g. `"Trt:Site"`).
#' @param reference The reference (control) level to compare every other level
#'   against, as a single character string. For an interaction `classify`, this
#'   is the `:`-joined cell label (e.g. `"A:X"`); when `by` is used it is a level
#'   of the remaining (non-`by`) factor. Each comparison is
#'   `mean(level) - mean(reference)`, so a positive estimate is "above the
#'   control".
#' @param adjust The method used to adjust p-values for multiplicity over the
#'   set of comparisons against the reference. Default `"dunnett"` performs the
#'   exact simultaneous two-sided Dunnett test (via the multivariate-t
#'   distribution). Any [stats::p.adjust.methods] value is also accepted (e.g.
#'   `"holm"`, `"bonferroni"`, `"BH"`); `"tukey"` is **not** valid here (use
#'   [multiple_comparisons()]). See Details.
#' @param by A character vector of one or more `classify` factors over which to
#'   split the comparisons. The same reference set is tested, and adjusted,
#'   independently within each level (or combination of levels) of `by`. Default
#'   `NULL`. See Details.
#' @param sig The significance level for the confidence intervals, numeric
#'   between 0 and 1. Default is 0.05.
#' @param include_means Logical. The predicted means are central to this
#'   display, so they are always included (`level1.mean`, `level2.mean`); setting
#'   `FALSE` is ignored with a warning.
#' @param descending Tri-state control of row ordering within each by-group.
#'   `NULL` (default) keeps prediction order; `FALSE` sorts ascending by
#'   estimate; `TRUE` sorts descending by estimate.
#' @param ... Other arguments passed to the model-specific prediction methods
#'   (e.g. ASReml-R `predict()` arguments).
#'
#' @details
#' ## Why Dunnett is the default
#' Comparing several treatments to one control has an *exact* simultaneous
#' procedure — Dunnett's test — which uses the joint multivariate-t distribution
#' of the comparisons (accounting for the correlation induced by the shared
#' control). It is more powerful than applying a generic adjustment, so it is the
#' default. The exact test requires a single (common) degrees-of-freedom; for
#' models that report comparison-specific df, `reference_comparisons()` falls
#' back to `"holm"` with a warning. Any [stats::p.adjust.methods] method may be
#' requested explicitly (a message notes that Dunnett is the exact option).
#'
#' The Dunnett correlation structure is reconstructed from the standard errors of
#' the means and the standard errors of differences that the prediction machinery
#' already returns, so the exact test is available for every supported model
#' engine. With `adjust = "dunnett"` the confidence intervals are the
#' *simultaneous* Dunnett intervals and therefore agree with the adjusted test
#' (an interval excludes zero exactly when the comparison is significant). With a
#' [stats::p.adjust()] method the intervals are per-comparison and may disagree
#' with the adjusted p-value.
#'
#' ## `by` semantics
#' `by` must be a subset of the `classify` factors. Within each group, the
#' reference and the compared levels reference the remaining (non-`by`) factor.
#' For example `classify = "Trt:Site"`, `by = "Site"`, `reference = "Control"`
#' compares every Trt level against Control *within each Site*, adjusted within
#' each Site. A group missing the reference, or with fewer than two levels, is
#' skipped with a warning.
#'
#' ## Transformations
#' Comparisons are reported on the model scale (a difference of transformed means
#' does not back-transform to a difference on the original scale); a warning is
#' issued if the response appears to be transformed in the model formula.
#'
#' @returns A `data.frame` of class `reference_comparisons` with one row per
#'   (group × non-reference level) and columns: any `by` column(s), `level1`,
#'   `level2` (the reference), `comparison`, `estimate`, `level1.mean`,
#'   `level2.mean` (the reference mean), `std.error`, `statistic`, `df`,
#'   `p.value` (adjusted), `conf.low` and `conf.high`. The reference level is not
#'   given its own row (its mean appears as `level2.mean` on every row, and in the
#'   `reference` attribute). Stored at full precision; rounding for display is
#'   controlled by [print.reference_comparisons()].
#'
#' @seealso [multiple_comparisons()] for all-pairs means and letters,
#'   [pairwise_comparisons()] for selected differences. For guidance on choosing
#'   between them and on multiplicity adjustments, see
#'   `vignette("choosing-multiple-comparisons", "biometryassist")`.
#'
#' @export
#'
#' @examples
#' dat.aov <- aov(weight ~ feed, data = chickwts)
#'
#' # Compare every feed against the "casein" control (exact Dunnett)
#' reference_comparisons(dat.aov, classify = "feed", reference = "casein")
reference_comparisons <- function(
	model.obj,
	classify,
	reference,
	adjust = "dunnett",
	by = NULL,
	sig = 0.05,
	include_means = TRUE,
	descending = NULL,
	...
) {
	# Validate the reference
	if (
		missing(reference) ||
			!is.character(reference) ||
			length(reference) != 1
	) {
		stop(
			"`reference` must be a single level label (character string).",
			call. = FALSE
		)
	}

	# Validate the adjustment method
	if (!is.character(adjust) || length(adjust) != 1) {
		stop("`adjust` must be a single character string.", call. = FALSE)
	}
	adjust <- tolower(adjust)
	if (adjust == "tukey") {
		stop(
			"`adjust = \"tukey\"` is not valid for reference_comparisons(): Tukey's ",
			"HSD is exact only for the complete set of all pairwise comparisons. ",
			"Use multiple_comparisons() for that.",
			call. = FALSE
		)
	}
	if (adjust != "dunnett" && adjust %notin% stats::p.adjust.methods) {
		stop(
			"Invalid `adjust` method. Must be \"dunnett\" or one of: ",
			paste(stats::p.adjust.methods, collapse = ", "),
			".",
			call. = FALSE
		)
	}
	if (adjust != "dunnett") {
		message(
			"Using adjust = \"",
			adjust,
			"\". For exact simultaneous control of all-vs-reference comparisons, ",
			"adjust = \"dunnett\" is the exact method."
		)
	}

	# The means are the point of this display; include_means is forced on
	if (!isTRUE(include_means)) {
		warning(
			"`include_means` is ignored (forced to TRUE) in reference_comparisons(): ",
			"the predicted means are central to the reference display.",
			call. = FALSE
		)
		include_means <- TRUE
	}

	# sig / classify / transformation checks (shared with multiple_comparisons())
	vars <- validate_inputs(sig, classify, model.obj, trans = NULL)

	# Predictions, SED matrix and degrees of freedom for the chosen engine
	result <- get_predictions(model.obj, classify, ...)
	pp <- result$predictions
	sed <- result$sed
	ndf <- result$df
	ylab <- result$ylab

	# Exact Dunnett needs a single common df; comparison-specific df (a matrix)
	# has no single multivariate-t df, so fall back to Holm.
	if (adjust == "dunnett" && is.matrix(ndf)) {
		warning(
			"Exact Dunnett requires a common degrees-of-freedom, but this model ",
			"provides comparison-specific df. Falling back to adjust = \"holm\".",
			call. = FALSE
		)
		adjust <- "holm"
	}

	# Resolve the `by` split and the remaining within-group factors
	if (!is.null(by)) {
		if (!all(by %in% vars)) {
			stop(
				"`by` must be a subset of the `classify` factor(s). Unknown: ",
				paste(setdiff(by, vars), collapse = ", "),
				".",
				call. = FALSE
			)
		}
		within_vars <- setdiff(vars, by)
		if (length(within_vars) == 0) {
			stop(
				"`by` consumes all `classify` factors, leaving nothing to compare ",
				"within each group.",
				call. = FALSE
			)
		}
	} else {
		within_vars <- vars
	}

	# Within-group label for every prediction row (interaction factors joined
	# with ":"); shared label builder with multiple_comparisons().
	within_label <- as.character(make_treatment_labels(
		pp,
		within_vars,
		sep = ":"
	))

	# The reference must exist among the (within-group) levels
	if (reference %notin% within_label) {
		stop(
			"`reference` level '",
			reference,
			"' not found among the levels of ",
			classify,
			".\nAvailable levels: ",
			paste(unique(within_label), collapse = ", "),
			".",
			call. = FALSE
		)
	}

	# Split rows into by-groups (or a single "All" group)
	if (!is.null(by)) {
		by_key <- interaction(pp[, by, drop = FALSE], drop = TRUE, sep = ":")
		group_levels <- levels(by_key)
	} else {
		by_key <- factor(rep("All", nrow(pp)))
		group_levels <- "All"
	}

	blocks <- list()
	for (g in group_levels) {
		idx <- which(by_key == g)
		group_labels <- within_label[idx]
		if (reference %notin% group_labels) {
			warning(
				"Reference level '",
				reference,
				"' is not present in group '",
				g,
				"'; skipping.",
				call. = FALSE
			)
			next
		}
		if (length(group_labels) < 2) {
			warning(
				"Group '",
				g,
				"' has fewer than 2 levels to compare; skipping.",
				call. = FALSE
			)
			next
		}
		# Each non-reference level vs the reference (reference on the RHS)
		others <- setdiff(group_labels, reference)
		group_pairs <- lapply(others, function(o) c(o, reference))
		blocks[[length(blocks) + 1]] <- build_pairwise_block(
			group_pairs,
			idx,
			group_labels,
			pp,
			sed,
			ndf,
			adjust,
			sig,
			by,
			descending,
			include_means
		)
	}

	if (length(blocks) == 0) {
		stop("No comparisons could be computed.", call. = FALSE)
	}

	out <- do.call(rbind, blocks)
	rownames(out) <- NULL

	class(out) <- c("reference_comparisons", "data.frame")
	attr(out, "sig_level") <- sig
	attr(out, "comparison_method") <- adjust
	attr(out, "classify") <- classify
	attr(out, "by") <- by
	attr(out, "ylab") <- ylab
	attr(out, "reference") <- reference

	return(out)
}


#' @rdname reference_comparisons
#'
#' @param x A `reference_comparisons` object.
#' @param decimals Number of decimal places to display. Default is 2. The
#'   p-value is shown to 3 significant figures (rather than rounded) so very
#'   small p-values do not collapse to zero.
#'
#' @returns `print.reference_comparisons()` invisibly returns `x`.
#'
#' @export
print.reference_comparisons <- function(x, decimals = 2, ...) {
	cat("Comparisons against a reference level\n")
	cat("Classify:", attr(x, "classify"), "\n")
	cat("Reference:", attr(x, "reference"), "\n")
	cat("Adjustment method:", attr(x, "comparison_method"), "\n")
	cat("Significance level:", attr(x, "sig_level"), "\n\n")

	out <- as.data.frame(x)
	round_cols <- intersect(
		c(
			"estimate",
			"level1.mean",
			"level2.mean",
			"std.error",
			"statistic",
			"df",
			"conf.low",
			"conf.high"
		),
		names(out)
	)
	out[round_cols] <- lapply(out[round_cols], round, decimals)
	if ("p.value" %in% names(out)) {
		# signif (not round) so very small p-values don't collapse to 0
		out$p.value <- signif(out$p.value, 3)
	}

	print(out, ...)
	invisible(x)
}
