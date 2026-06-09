#' Pairwise comparisons of predicted means
#'
#' Test a chosen set of pairwise differences between the predicted means of a
#' fitted model, with multiplicity adjustment over the chosen set and optional
#' splitting into independent subgroups. Unlike [multiple_comparisons()], which
#' is means-centric and summarises *all* pairwise comparisons via a compact
#' letter display, `pairwise_comparisons()` is difference-centric: it returns a
#' tidy table with one row per requested comparison (estimate, standard error,
#' statistic, degrees of freedom and adjusted p-value). This honestly represents
#' selective and irregular comparison sets, for which letter groupings are not
#' valid.
#'
#' It works for every model supported by [multiple_comparisons()], because a
#' pairwise difference needs only the standard error of the difference (SED)
#' matrix that the prediction machinery already returns.
#'
#' @param model.obj An ASReml-R, `aov`, `lm`, `lme` ([nlme::lme()]) or `lmerMod`
#'   ([lme4::lmer()]) model object.
#' @param classify Name of the predictor variable(s) to compare, as a string.
#'   Interactions are specified with `:` (e.g. `"Trt:Site"`).
#' @param pairs The comparisons to test. `NULL` (default) tests all pairwise
#'   comparisons. Otherwise either a character vector of `"level1-level2"`
#'   labels (levels of an interaction joined by `:`, the two sides of a pair
#'   separated by `-`, e.g. `"A:X-B:Y"`), or a list of length-2 character
#'   vectors (e.g. `list(c("A:X", "B:Y"))`). The list form is required if any
#'   factor level itself contains a `-`. See Details.
#' @param adjust The method used to adjust p-values for multiplicity over the
#'   chosen set, passed to [stats::p.adjust()]. Default is `"holm"`. Any
#'   [stats::p.adjust.methods] value is accepted. `"tukey"` is **not** valid
#'   here (it is exact only for the complete set of all pairwise comparisons —
#'   use [multiple_comparisons()] for that).
#' @param by A character vector of one or more `classify` factors over which to
#'   split the comparisons. The same `pairs` set is tested, and adjusted,
#'   independently within each level (or combination of levels) of `by`, with no
#'   pooling across groups. Default `NULL`. See Details.
#' @param sig The significance level for the confidence intervals, numeric
#'   between 0 and 1. Default is 0.05.
#' @param descending Tri-state control of row ordering within each by-group.
#'   `NULL` (default) keeps the input order of `pairs`; `FALSE` sorts ascending
#'   by estimate; `TRUE` sorts descending by estimate.
#' @param ... Other arguments passed to the model-specific prediction methods
#'   (e.g. ASReml-R `predict()` arguments).
#'
#' @details
#' ## `pairs` syntax and sign convention
#' The estimate for a pair is `mean(level1) - mean(level2)`, in the order
#' written (`"A-B"` gives A − B). The `level1` and `level2` columns make the sign
#' unambiguous. With `:` joining interaction cells and `-` separating the two
#' sides of a pair, the string form is unambiguous only when no factor level
#' contains a `-`; a level containing `-` referenced via the string form is an
#' error directing you to the list form. Reversed or duplicated pairs
#' (`"A-B"` and `"B-A"`) are de-duplicated with a warning, since duplicates would
#' inflate the adjustment family.
#'
#' ## `by` semantics
#' `by` must be a subset of the `classify` factors. Within each group, pair
#' labels reference the remaining (non-`by`) factor levels. For example,
#' `classify = "Trt:Site"`, `by = "Site"`, `pairs = "A-B"` compares Trt levels A
#' and B within each Site. A group with fewer than two levels is skipped with a
#' warning.
#'
#' ## Transformations
#' Comparisons are reported on the model scale. A difference of transformed means
#' does not back-transform to a difference on the original scale (for log/logit
#' it is a ratio), so back-transformation is not attempted; a warning is issued
#' if the response appears to be transformed in the model formula.
#'
#' ## Confidence intervals
#' The per-comparison confidence intervals are *not* simultaneity-adjusted: as
#' with the confidence-interval/letter note in [multiple_comparisons()], an
#' interval may exclude zero while the adjusted p-value is `>= sig`.
#'
#' @returns A `data.frame` of class `pairwise_comparisons` with one row per
#'   (group × comparison) and columns: any `by` column(s), `level1`, `level2`,
#'   `comparison`, `estimate`, `std.error`, `statistic`, `df`, `p.value`
#'   (adjusted), `conf.low` and `conf.high`. Stored at full precision; rounding
#'   for display is controlled by [print.pairwise_comparisons()].
#'
#' @seealso [multiple_comparisons()] for means-and-letters output.
#'
#' @importFrom stats pt p.adjust qt p.adjust.methods
#' @export
#'
#' @examples
#' dat.aov <- aov(Petal.Width ~ Species, data = iris)
#'
#' # All pairwise comparisons (Holm-adjusted by default)
#' pairwise_comparisons(dat.aov, classify = "Species")
#'
#' # A selected subset
#' pairwise_comparisons(
#'     dat.aov,
#'     classify = "Species",
#'     pairs = c("setosa-versicolor", "setosa-virginica")
#' )
pairwise_comparisons <- function(
	model.obj,
	classify,
	pairs = NULL,
	adjust = "holm",
	by = NULL,
	sig = 0.05,
	descending = NULL,
	...
) {
	# Validate the adjustment method
	if (!is.character(adjust) || length(adjust) != 1) {
		stop("`adjust` must be a single character string.", call. = FALSE)
	}
	adjust <- tolower(adjust)
	if (adjust == "tukey") {
		stop(
			"`adjust = \"tukey\"` is not valid for pairwise_comparisons(): Tukey's ",
			"HSD is exact only for the complete set of all pairwise comparisons. ",
			"Use multiple_comparisons() for that, or choose another `adjust` ",
			"method (e.g. \"holm\", \"bonferroni\", \"BH\").",
			call. = FALSE
		)
	}
	if (!(adjust %in% stats::p.adjust.methods)) {
		stop(
			"Invalid `adjust` method. Must be one of: ",
			paste(stats::p.adjust.methods, collapse = ", "),
			".",
			call. = FALSE
		)
	}

	# sig / classify / transformation checks (shared with multiple_comparisons())
	vars <- validate_inputs(sig, classify, model.obj, trans = NULL)

	# Predictions, SED matrix and degrees of freedom for the chosen engine
	result <- get_predictions(model.obj, classify, ...)
	pp <- result$predictions
	sed <- result$sed
	ndf <- result$df
	ylab <- result$ylab

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

	# Within-group label for every prediction row
	within_label <- build_pairwise_labels(pp, within_vars)

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
		if (length(idx) < 2) {
			warning(
				"Group '",
				g,
				"' has fewer than 2 levels to compare; skipping.",
				call. = FALSE
			)
			next
		}
		group_labels <- within_label[idx]
		group_pairs <- parse_pairs(pairs, group_labels)
		if (length(group_pairs) == 0) {
			next
		}
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
			descending
		)
	}

	if (length(blocks) == 0) {
		stop("No comparisons could be computed.", call. = FALSE)
	}

	out <- do.call(rbind, blocks)
	rownames(out) <- NULL

	class(out) <- c("pairwise_comparisons", "data.frame")
	attr(out, "sig_level") <- sig
	attr(out, "comparison_method") <- adjust
	attr(out, "classify") <- classify
	attr(out, "by") <- by
	attr(out, "ylab") <- ylab

	return(out)
}


#' Build `:`-joined labels for prediction rows
#'
#' @param pp The predictions data frame.
#' @param vars Character vector of factor column names to join.
#' @noRd
build_pairwise_labels <- function(pp, vars) {
	if (length(vars) == 1) {
		as.character(pp[[vars]])
	} else {
		apply(pp[, vars, drop = FALSE], 1, paste, collapse = ":")
	}
}


#' Normalise the `pairs` argument to a list of ordered level pairs
#'
#' Accepts the string form (`"A:X-B:Y"`) or the list form
#' (`list(c("A:X", "B:Y"))`) and returns a list of length-2 character vectors,
#' validated against `labels`, de-duplicated order-insensitively.
#'
#' @param pairs The user-supplied `pairs` argument.
#' @param labels Character vector of valid (within-group) level labels.
#' @noRd
parse_pairs <- function(pairs, labels) {
	if (is.null(pairs)) {
		return(utils::combn(labels, 2, simplify = FALSE))
	}

	if (is.list(pairs)) {
		raw <- lapply(pairs, function(p) {
			if (!is.character(p) || length(p) != 2) {
				stop(
					"Each element of `pairs` (list form) must be a character vector ",
					"of length 2.",
					call. = FALSE
				)
			}
			as.character(p)
		})
	} else if (is.character(pairs)) {
		raw <- lapply(pairs, function(s) {
			parts <- trimws(strsplit(s, "-", fixed = TRUE)[[1]])
			if (length(parts) != 2) {
				stop(
					"Could not unambiguously parse pair \"",
					s,
					"\" (expected exactly two levels separated by '-'). If a factor ",
					"level contains '-', use the list form, e.g. ",
					"list(c(\"level1\", \"level2\")).",
					call. = FALSE
				)
			}
			parts
		})
	} else {
		stop(
			"`pairs` must be NULL, a character vector, or a list of length-2 ",
			"character vectors.",
			call. = FALSE
		)
	}

	# Existence and distinctness checks
	unknown <- setdiff(unique(unlist(raw)), labels)
	if (length(unknown) > 0) {
		stop(
			"Unknown level(s) in `pairs`: ",
			paste(unknown, collapse = ", "),
			".\nAvailable levels: ",
			paste(labels, collapse = ", "),
			".",
			call. = FALSE
		)
	}
	for (p in raw) {
		if (p[1] == p[2]) {
			stop(
				"A pair must reference two distinct levels (got \"",
				p[1],
				"\" twice).",
				call. = FALSE
			)
		}
	}

	# De-duplicate order-insensitively, keeping first occurrence and its direction
	seen <- character(0)
	out <- list()
	dups <- character(0)
	for (p in raw) {
		key <- paste(sort(p), collapse = "\r")
		if (key %in% seen) {
			dups <- c(dups, paste(p, collapse = "-"))
			next
		}
		seen <- c(seen, key)
		out[[length(out) + 1]] <- p
	}
	if (length(dups) > 0) {
		warning(
			"Duplicate or reversed pair(s) removed: ",
			paste(dups, collapse = ", "),
			".",
			call. = FALSE
		)
	}

	out
}


#' Compute one by-group block of the pairwise comparison table
#'
#' @param group_pairs List of length-2 character vectors (within-group labels).
#' @param idx Global row indices of this group in `pp`.
#' @param group_labels Within-group labels for `idx` (same order as `idx`).
#' @param pp,sed,ndf Predictions, SED matrix, degrees of freedom from
#'   `get_predictions()`.
#' @param adjust,sig,by,descending As in [pairwise_comparisons()].
#' @noRd
build_pairwise_block <- function(
	group_pairs,
	idx,
	group_labels,
	pp,
	sed,
	ndf,
	adjust,
	sig,
	by,
	descending
) {
	# Map within-group labels to their global row index
	lab2idx <- stats::setNames(idx, group_labels)
	i_idx <- vapply(group_pairs, function(p) lab2idx[[p[1]]], integer(1))
	j_idx <- vapply(group_pairs, function(p) lab2idx[[p[2]]], integer(1))

	pv <- pp$predicted.value
	est <- pv[i_idx] - pv[j_idx]

	se <- if (!is.null(dim(sed))) {
		as.numeric(sed[cbind(i_idx, j_idx)])
	} else {
		rep_len(sed, length(est))
	}
	df_ij <- if (is.matrix(ndf)) {
		as.numeric(ndf[cbind(i_idx, j_idx)])
	} else {
		rep_len(ndf, length(est))
	}

	tstat <- est / se
	p_raw <- 2 * stats::pt(-abs(tstat), df = df_ij)
	p_adj <- stats::p.adjust(p_raw, method = adjust)

	crit <- stats::qt(1 - sig / 2, df_ij)

	block <- data.frame(
		level1 = vapply(group_pairs, `[`, character(1), 1),
		level2 = vapply(group_pairs, `[`, character(1), 2),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
	block$comparison <- paste(block$level1, "-", block$level2)
	block$estimate <- est
	block$std.error <- se
	block$statistic <- tstat
	block$df <- df_ij
	block$p.value <- p_adj
	block$conf.low <- est - crit * se
	block$conf.high <- est + crit * se

	# Prepend the by-column(s), taken from this group's (constant) values
	if (!is.null(by)) {
		by_df <- pp[rep(idx[1], nrow(block)), by, drop = FALSE]
		rownames(by_df) <- NULL
		block <- cbind(by_df, block)
	}

	# Order within the group block
	if (!is.null(descending)) {
		block <- block[
			order(block$estimate, decreasing = descending),
			,
			drop = FALSE
		]
	}

	block
}


#' @rdname pairwise_comparisons
#'
#' @param x A `pairwise_comparisons` object.
#' @param decimals Number of decimal places to display. Default is 2. The
#'   p-value is shown to 3 significant figures (rather than rounded) so very
#'   small p-values do not collapse to zero.
#'
#' @returns `print.pairwise_comparisons()` invisibly returns `x`.
#'
#' @export
print.pairwise_comparisons <- function(x, decimals = 2, ...) {
	cat("Pairwise comparisons of means\n")
	cat("Classify:", attr(x, "classify"), "\n")
	cat("Adjustment method:", attr(x, "comparison_method"), "\n")
	cat("Significance level:", attr(x, "sig_level"), "\n\n")

	out <- as.data.frame(x)
	round_cols <- intersect(
		c("estimate", "std.error", "statistic", "df", "conf.low", "conf.high"),
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
