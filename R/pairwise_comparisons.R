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
#' @param model.obj An `asreml`, `aov`, `lm`, `lme` ([nlme::lme()]) or `lmerMod`
#'   ([lme4::lmer()]) model object.
#' @param classify Name of the predictor variable(s) to compare, as a string.
#'   Interactions are specified with `:` (e.g. `"Trt:Site"`).
#' @param pairs The comparisons to test. `NULL` (default) tests all pairwise
#'   comparisons. Otherwise either a character vector of `"level1-level2"`
#'   labels (levels of an interaction joined by `:`, the two sides of a pair
#'   separated by `-`, e.g. `"A:X-B:Y"`), or a list of length-2 character
#'   vectors (e.g. `list(c("A:X", "B:Y"))`). The list form is required if any
#'   factor level itself contains a `-`. See Details.
#' @param contrasts An optional named list of general linear contrasts to test
#'   *instead of* `pairs`. Each element is a named numeric vector of coefficients
#'   keyed by level label (e.g.
#'   `list("A vs B & C" = c(A = 1, B = -0.5, C = -0.5))`), and the list names
#'   become the `comparison` labels. The estimate is the corresponding linear
#'   combination of the predicted means. Mutually exclusive with `pairs`;
#'   coefficients should sum to zero (a warning is issued otherwise).
#'   `include_means` does not apply to this form. See Details for how the
#'   standard error and degrees of freedom are obtained.
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
#' @param include_means Logical; if `TRUE` (default) the predicted mean of each
#'   side of the comparison is included as `level1.mean` and `level2.mean`
#'   columns, immediately after `estimate` (of which they are the decomposition:
#'   `estimate = level1.mean - level2.mean`). Set `FALSE` for the differences
#'   only.
#' @param descending Tri-state control of row ordering within each by-group.
#'   `NULL` (default) keeps the input order of `pairs`; `FALSE` sorts ascending
#'   by estimate; `TRUE` sorts descending by estimate. Note this orders by the
#'   comparison *estimate* (the difference), unlike [multiple_comparisons()]
#'   which orders by the predicted *mean* — appropriate here as the output is
#'   difference-centric.
#' @param ... Other arguments passed to the model-specific prediction methods
#'   (e.g. ASReml-R `predict()` arguments).
#'
#' @details
#' ## Relationship to `multiple_comparisons()`
#' The two functions share the same predicted means and standard errors of
#' differences, so for a given `adjust` method they report the same comparisons.
#' Testing all pairs here (`pairs = NULL`) with a particular `adjust` yields the
#' same adjusted p-values as [multiple_comparisons()] with the same `adjust` —
#' the difference is presentation (a tidy table of differences and a forest plot,
#' versus means, confidence intervals and letter groupings). The exception is
#' Tukey's HSD, which is exact only for the complete set of all pairwise
#' comparisons and so is the domain of [multiple_comparisons()]; `adjust =
#' "tukey"` is therefore not accepted here.
#'
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
#' For an interaction `classify`, the `:`-joined components of each level label
#' must be in the same order as `classify` (e.g. `"A:X"` for
#' `classify = "Trt:Site"`, not `"X:A"`); the same applies to the level names
#' used in `contrasts`. A label that names no existing cell is rejected with the
#' list of available levels, but a mis-ordered label that happens to name
#' another valid cell is used silently — so when the factors share level names,
#' check the component order matches `classify`.
#'
#' ## `by` semantics
#' `by` must be a subset of the `classify` factors. Within each group, pair
#' labels reference the remaining (non-`by`) factor levels. For example,
#' `classify = "Trt:Site"`, `by = "Site"`, `pairs = "A-B"` compares Trt levels A
#' and B within each Site. A group with fewer than two levels is skipped with a
#' warning. In an unbalanced design a requested comparison may reference a level
#' that is absent from some groups: such a comparison is skipped (with a warning)
#' in the groups where it cannot be computed and reported in those where it can.
#' A level that is absent from *every* group — or that was aliased — is instead
#' an error, since that indicates a mistake rather than an incomplete design.
#'
#' ## Standard error and degrees of freedom for `contrasts`
#' For a general contrast the standard error and degrees of freedom depend on
#' the model engine:
#' * For models predicted via `emmeans` (`aov`, `lm`, [nlme::lme()],
#'   [lme4::lmer()], and `aov` with `Error()` strata) the estimate, standard
#'   error and degrees of freedom are obtained directly from
#'   [emmeans::contrast()] on the model's reference grid. The degrees of freedom
#'   are therefore the *exact* contrast df for that engine (Satterthwaite or
#'   Kenward-Roger for mixed models, containment for `aov`/`Error()`), including
#'   for contrasts spanning more than two levels — these cannot be recovered
#'   from the SED matrix alone.
#' * For `asreml` models the contrast variance is computed from the prediction
#'   error covariance (reconstructed from the SED matrix), and the degrees of
#'   freedom are the term's denominator df from `asreml::wald(denDF = "default")`
#'   (a single Kenward-Roger-style value shared by all contrasts within the
#'   term, as ASReml-R does not provide a per-contrast approximate df).
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
#' interval may exclude zero while the adjusted p-value is `>= sig`. When this
#' happens a one-time `message()` notes it (suppressible with
#' [suppressMessages()]).
#'
#' @returns A `data.frame` of class `pairwise_comparisons` with one row per
#'   (group × comparison) and columns: any `by` column(s), `level1`, `level2`,
#'   `comparison`, `estimate`, (optionally `level1.mean` and `level2.mean`),
#'   `std.error`, `statistic`, `df`, `p.value` (adjusted), `conf.low` and
#'   `conf.high`. Stored at full precision; rounding for display is controlled
#'   by [print.pairwise_comparisons()]. For the `contrasts` form, the
#'   `level1`/`level2` and mean columns are omitted and `comparison` holds the
#'   contrast name. If any levels were aliased (not estimable) in the model they
#'   are dropped from the comparisons (with a warning) and recorded in an
#'   `aliased` attribute.
#'
#' @seealso [multiple_comparisons()] for means-and-letters output. For guidance
#'   on choosing between the two and on multiplicity adjustments, see
#'   `vignette("choosing-multiple-comparisons", "biometryassist")`.
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
#'
#' # A general (non-pairwise) contrast: setosa vs the average of the others
#' pairwise_comparisons(
#'     dat.aov,
#'     classify = "Species",
#'     contrasts = list(
#'         "setosa vs rest" = c(setosa = 1, versicolor = -0.5, virginica = -0.5)
#'     )
#' )
pairwise_comparisons <- function(
	model.obj,
	classify,
	pairs = NULL,
	contrasts = NULL,
	adjust = "holm",
	by = NULL,
	sig = 0.05,
	include_means = TRUE,
	descending = NULL,
	...
) {
	# Catch misspelled arguments silently swallowed by `...` (consistent with
	# multiple_comparisons()). Any dot not consumed by the prediction method
	# (e.g. ASReml-R predict() arguments) is an error rather than ignored.
	rlang::check_dots_used()

	# `pairs` and `contrasts` are two ways to specify the same engine
	if (!is.null(pairs) && !is.null(contrasts)) {
		stop("Supply only one of `pairs` or `contrasts`, not both.", call. = FALSE)
	}

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
	if (adjust == "dunnett") {
		stop(
			"`adjust = \"dunnett\"` is not valid for pairwise_comparisons(): the ",
			"Dunnett test compares every level against a single control. Use ",
			"reference_comparisons() for that.",
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
	# Levels that were aliased (not estimable) and dropped by process_aliased();
	# used to give a clear error if the user names one, and stored on the output.
	aliased <- result$aliased_names

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

	# For general contrasts on an emmeans-backed engine (everything except
	# asreml), delegate the estimate/SE/df to emmeans on its reference grid so
	# the degrees of freedom are exact (Satterthwaite / Kenward-Roger) rather
	# than reconstructed from the SEDs. `emm_info` carries the grid, the map from
	# prediction rows to grid rows (matched by full classify label so it is
	# robust to aliased rows having been dropped), and the grid size. It is NULL
	# for asreml (no grid), where build_contrast_block() reconstructs from SEDs.
	emm_info <- NULL
	if (!is.null(contrasts) && !is.null(result$emmeans_grid)) {
		grid_df <- as.data.frame(result$emmeans_grid)
		grid_label <- as.character(make_treatment_labels(grid_df, vars, sep = ":"))
		pp_label <- as.character(make_treatment_labels(pp, vars, sep = ":"))
		emm_info <- list(
			grid = result$emmeans_grid,
			pp_to_grid = match(pp_label, grid_label),
			n = nrow(grid_df)
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

	# Validate the user-named levels once, against the full set of estimable
	# levels: a typo or an aliased level errors here (clear, up front). The
	# per-group loop below then only *warns and skips* comparisons a particular
	# by-group cannot compute (an unbalanced design where a level is absent from
	# some group), rather than failing the whole call.
	global_labels <- unique(within_label)
	all_pairs <- NULL
	all_contrasts <- NULL
	if (!is.null(contrasts)) {
		all_contrasts <- parse_contrasts(contrasts, global_labels, aliased)
	} else if (!is.null(pairs)) {
		all_pairs <- parse_pairs(pairs, global_labels, aliased)
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

		if (!is.null(contrasts)) {
			# Keep only contrasts whose every level is present in this group.
			keep <- vapply(
				all_contrasts,
				function(co) all(names(co) %in% group_labels),
				logical(1)
			)
			if (any(!keep)) {
				warning(
					"In group '",
					g,
					"', skipped contrast(s) referencing levels not present there: ",
					paste(names(all_contrasts)[!keep], collapse = ", "),
					".",
					call. = FALSE
				)
			}
			group_contrasts <- all_contrasts[keep]
			if (length(group_contrasts) == 0) {
				next
			}
			blocks[[length(blocks) + 1]] <- build_contrast_block(
				group_contrasts,
				idx,
				group_labels,
				pp,
				sed,
				ndf,
				adjust,
				sig,
				by,
				descending,
				emm_info
			)
		} else {
			if (is.null(pairs)) {
				# Default: all pairwise comparisons within this group.
				group_pairs <- parse_pairs(NULL, group_labels)
			} else {
				# Keep only pairs whose both levels are present in this group.
				keep <- vapply(
					all_pairs,
					function(p) all(p %in% group_labels),
					logical(1)
				)
				if (any(!keep)) {
					skipped <- vapply(
						all_pairs[!keep],
						function(p) paste(p, collapse = " - "),
						character(1)
					)
					warning(
						"In group '",
						g,
						"', skipped comparison(s) referencing levels not present ",
						"there: ",
						paste(skipped, collapse = ", "),
						".",
						call. = FALSE
					)
				}
				group_pairs <- all_pairs[keep]
			}
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
				descending,
				include_means
			)
		}
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
	attr(out, "comparison_type") <- if (!is.null(contrasts)) {
		"contrasts"
	} else {
		"pairs"
	}
	if (!is.null(aliased)) {
		attr(out, "aliased") <- as.character(aliased)
	}

	# Nudge if any per-comparison CI disagrees with its adjusted p-value.
	note_ci_padjust_mismatch(out, sig, adjust)

	return(out)
}


#' Error on level labels that are not among the estimable levels
#'
#' Distinguishes levels that were *aliased* (present in the data but not
#' estimable, so dropped from the predictions by `process_aliased()`) from
#' levels that are genuinely unknown, and reports each group with the
#' appropriate explanation.
#'
#' @param unknown Character vector of requested labels not found in `labels`.
#' @param labels Character vector of valid (estimable) level labels.
#' @param aliased Character vector of aliased level labels (or `NULL`).
#' @param context The argument name to name in the message (e.g. `"pairs"`).
#' @noRd
stop_unknown_levels <- function(unknown, labels, aliased, context) {
	aliased_hit <- intersect(unknown, aliased)
	truly <- setdiff(unknown, aliased)
	msg <- character(0)
	if (length(aliased_hit) > 0) {
		msg <- c(
			msg,
			paste0(
				"Aliased (not estimable) level(s) in `",
				context,
				"`: ",
				paste(aliased_hit, collapse = ", "),
				". These levels were aliased in the model and removed from the ",
				"predictions, so they cannot be compared."
			)
		)
	}
	if (length(truly) > 0) {
		msg <- c(
			msg,
			paste0(
				"Unknown level(s) in `",
				context,
				"`: ",
				paste(truly, collapse = ", "),
				".\nAvailable levels: ",
				paste(labels, collapse = ", "),
				"."
			)
		)
	}
	stop(paste(msg, collapse = "\n"), call. = FALSE)
}


#' Normalise the `pairs` argument to a list of ordered level pairs
#'
#' Accepts the string form (`"A:X-B:Y"`) or the list form
#' (`list(c("A:X", "B:Y"))`) and returns a list of length-2 character vectors,
#' validated against `labels`, de-duplicated order-insensitively.
#'
#' @param pairs The user-supplied `pairs` argument.
#' @param labels Character vector of valid (within-group) level labels.
#' @param aliased Character vector of aliased level labels (or `NULL`), used to
#'   give a clearer error when a requested level was aliased rather than unknown.
#' @noRd
parse_pairs <- function(pairs, labels, aliased = NULL) {
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
		stop_unknown_levels(unknown, labels, aliased, "pairs")
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
#' @param adjust,sig,by,descending,include_means As in [pairwise_comparisons()].
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
	descending,
	include_means = TRUE
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

	if (adjust == "dunnett") {
		# Exact simultaneous all-vs-reference inference (the CIs are then the
		# simultaneous Dunnett intervals, which agree with the adjusted test).
		# This branch is only ever reached via reference_comparisons():
		# pairwise_comparisons() explicitly rejects adjust = "dunnett" (directing
		# the user to reference_comparisons()), so build_pairwise_block() never
		# sees it from the pairwise entry point. The engine lives here because
		# reference_comparisons() reuses build_pairwise_block() for its
		# all-vs-reference set.
		dn <- dunnett_adjust(i_idx, j_idx, se, df_ij, tstat, pp, sed, sig)
		p_adj <- dn$p.value
		crit <- dn$crit
	} else {
		p_raw <- 2 * stats::pt(-abs(tstat), df = df_ij)
		p_adj <- stats::p.adjust(p_raw, method = adjust)
		crit <- stats::qt(1 - sig / 2, df_ij)
	}

	block <- data.frame(
		level1 = vapply(group_pairs, `[`, character(1), 1),
		level2 = vapply(group_pairs, `[`, character(1), 2),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
	block$comparison <- paste(block$level1, "-", block$level2)
	block$estimate <- est
	if (include_means) {
		block$level1.mean <- pv[i_idx]
		block$level2.mean <- pv[j_idx]
	}
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


#' Exact two-sided Dunnett adjustment for a block of all-vs-reference contrasts
#'
#' Reconstructs the variance-covariance matrix `V` of the predicted means from
#' the per-mean SEs (diagonal, `V_ii = std.error_i^2`) and the SED matrix
#' (off-diagonal, `V_ij = (V_ii + V_jj - SED_ij^2) / 2`), forms the correlation
#' matrix of the contrasts, and uses the multivariate-t distribution
#' (`mvtnorm`) for exact simultaneous two-sided adjusted p-values and a single
#' simultaneous critical value for the confidence intervals. Validated to
#' machine precision against `emmeans` `trt.vs.ctrl` with `adjust = "mvt"`.
#'
#' Assumes a single (effectively integer) degrees-of-freedom across the block;
#' callers must fall back to another method when df vary by comparison.
#'
#' @param i_idx,j_idx Global prediction-row indices for each contrast's level1
#'   and level2.
#' @param se,df_ij,tstat Per-contrast SED, degrees of freedom and t-statistic.
#' @param pp,sed Predictions data frame and SED matrix from `get_predictions()`.
#' @param sig Significance level.
#' @noRd
dunnett_adjust <- function(i_idx, j_idx, se, df_ij, tstat, pp, sed, sig) {
	if (!requireNamespace("mvtnorm", quietly = TRUE)) {
		stop(
			"Package 'mvtnorm' is required for adjust = \"dunnett\". Install it ",
			"with install.packages(\"mvtnorm\"), or choose another `adjust` method.",
			call. = FALSE
		)
	}

	m <- length(tstat)
	df_exact <- df_ij[1]

	# Single comparison: no multiplicity -> exact two-sided t on the exact df.
	# stats::pt/qt accept a fractional df, so don't round here (mvtnorm below
	# does require an integer, but it isn't used in this branch).
	if (m == 1) {
		return(list(
			p.value = 2 * stats::pt(-abs(tstat), df_exact),
			crit = stats::qt(1 - sig / 2, df_exact)
		))
	}

	# mvtnorm::pmvt/qmvt require an INTEGER df for the multivariate t, so a
	# fractional denominator df (e.g. an asreml Kenward-Roger denDF) is rounded to
	# the nearest integer. aov/lm/lme have integer df and lmer/aov+Error fall back
	# to Holm, so in practice this only affects an asreml fractional denDF.
	df0 <- as.integer(round(df_exact))

	# Reconstruct V over the involved means
	u <- sort(unique(c(i_idx, j_idx)))
	V <- reconstruct_vcov(u, pp, sed)
	key <- function(g) match(g, u)

	# Correlation matrix of the (level1 - level2) contrasts
	R <- matrix(1, m, m)
	for (k in seq_len(m)) {
		ik <- key(i_idx[k])
		jk <- key(j_idx[k])
		for (l in seq_len(m)) {
			il <- key(i_idx[l])
			jl <- key(j_idx[l])
			cov_kl <- V[ik, il] - V[ik, jl] - V[jk, il] + V[jk, jl]
			R[k, l] <- cov_kl / (se[k] * se[l])
		}
	}
	R <- (R + t(R)) / 2
	diag(R) <- 1

	# Exact two-sided adjusted p: P(max|T| >= |t_k|)
	p <- vapply(
		seq_len(m),
		function(k) {
			cc <- abs(tstat[k])
			1 -
				as.numeric(mvtnorm::pmvt(
					lower = rep(-cc, m),
					upper = rep(cc, m),
					df = df0,
					corr = R
				))
		},
		numeric(1)
	)
	p <- pmin(pmax(p, 0), 1)

	# Simultaneous two-sided critical value for the confidence intervals
	cstar <- mvtnorm::qmvt(
		1 - sig,
		tail = "both.tails",
		df = df0,
		corr = R
	)$quantile

	list(p.value = p, crit = rep_len(cstar, m))
}


#' Reconstruct the variance-covariance matrix of predicted means
#'
#' From the per-mean SEs (diagonal, `V_ii = std.error_i^2`) and the SED matrix
#' (off-diagonal, `V_ij = (V_ii + V_jj - SED_ij^2) / 2`). Exact: the SEs and SEDs
#' are derived from the same fitted-model prediction covariance.
#'
#' @param u Integer vector of (global) prediction-row indices to include.
#' @param pp,sed Predictions data frame and SED matrix from `get_predictions()`.
#' @return A `length(u)` x `length(u)` covariance matrix, ordered as `u`.
#' @noRd
reconstruct_vcov <- function(u, pp, sed) {
	vd <- pp$std.error[u]^2
	V <- diag(vd, nrow = length(u))
	if (length(u) > 1) {
		for (a in seq_len(length(u) - 1)) {
			for (b in (a + 1):length(u)) {
				vab <- (vd[a] + vd[b] - as.numeric(sed[u[a], u[b]])^2) / 2
				V[a, b] <- V[b, a] <- vab
			}
		}
	}
	V
}


#' Normalise and validate the `contrasts` argument
#'
#' @param contrasts A named list of named numeric coefficient vectors.
#' @param labels Character vector of valid (within-group) level labels.
#' @param aliased Character vector of aliased level labels (or `NULL`), used to
#'   give a clearer error when a requested level was aliased rather than unknown.
#' @noRd
parse_contrasts <- function(contrasts, labels, aliased = NULL) {
	if (
		!is.list(contrasts) ||
			length(contrasts) == 0 ||
			is.null(names(contrasts)) ||
			any(!nzchar(names(contrasts)))
	) {
		stop(
			"`contrasts` must be a non-empty named list of named numeric ",
			"coefficient vectors, e.g. ",
			"list(\"A vs B&C\" = c(A = 1, B = -0.5, C = -0.5)).",
			call. = FALSE
		)
	}
	lapply(contrasts, function(co) {
		if (!is.numeric(co) || is.null(names(co)) || any(!nzchar(names(co)))) {
			stop(
				"Each contrast must be a named numeric vector whose names are level ",
				"labels.",
				call. = FALSE
			)
		}
		unknown <- setdiff(names(co), labels)
		if (length(unknown) > 0) {
			stop_unknown_levels(unknown, labels, aliased, "contrasts")
		}
		if (abs(sum(co)) > 1e-8) {
			warning(
				"Coefficients of a contrast do not sum to zero (sum = ",
				signif(sum(co), 3),
				"); it is evaluated as a general linear combination.",
				call. = FALSE
			)
		}
		co
	})
}


#' Compute one by-group block of general linear contrasts
#'
#' Each contrast is a named numeric coefficient vector over levels. The estimate
#' is `c' tau`, its variance `c' V c` (with `V` reconstructed from the SED matrix
#' and per-mean SEs via `reconstruct_vcov()`), and the test a two-sided t with
#' multiplicity-adjusted p-values over the group's family.
#'
#' @param group_contrasts List of named numeric coefficient vectors.
#' @param idx Global row indices of this group in `pp`.
#' @param group_labels Within-group labels for `idx`.
#' @param pp,sed,ndf,adjust,sig,by,descending As in [pairwise_comparisons()].
#' @noRd
build_contrast_block <- function(
	group_contrasts,
	idx,
	group_labels,
	pp,
	sed,
	ndf,
	adjust,
	sig,
	by,
	descending,
	emm_info = NULL
) {
	lab2idx <- stats::setNames(idx, group_labels)
	pv <- pp$predicted.value
	n <- length(group_contrasts)

	est <- numeric(n)
	se <- numeric(n)
	df_c <- numeric(n)

	for (k in seq_len(n)) {
		co <- group_contrasts[[k]]
		gi <- as.integer(lab2idx[names(co)])
		w <- as.numeric(co)

		if (!is.null(emm_info)) {
			# emmeans-backed engine: delegate to emmeans on the reference grid so
			# estimate, SE and (crucially) the degrees of freedom are exact for an
			# arbitrary linear contrast, including >2-level contrasts on mixed
			# models where the df is Satterthwaite / Kenward-Roger and cannot be
			# recovered from the SEDs. Coefficients are placed at the grid rows for
			# this group's levels (zero elsewhere); adjust = "none" because the
			# multiplicity adjustment is applied below over our chosen family.
			coef_full <- numeric(emm_info$n)
			coef_full[emm_info$pp_to_grid[gi]] <- w
			ct <- as.data.frame(emmeans::contrast(
				emm_info$grid,
				method = stats::setNames(list(coef_full), names(group_contrasts)[k]),
				adjust = "none"
			))
			est[k] <- ct$estimate
			se[k] <- ct$SE
			df_c[k] <- ct$df
		} else {
			# asreml (no grid): reconstruct from the SEDs. df is the model's single
			# residual df (scalar for asreml). Floor c'Vc at zero so a contrast with
			# ~0 true variance rounding just below zero gives se = 0, not a NaN; the
			# reconstructed V is the exact (PSD) model covariance, so this only ever
			# floors floating-point rounding noise.
			est[k] <- sum(w * pv[gi])
			u <- sort(unique(gi))
			V <- reconstruct_vcov(u, pp, sed)
			wal <- numeric(length(u))
			pos <- match(gi, u)
			for (q in seq_along(gi)) {
				wal[pos[q]] <- wal[pos[q]] + w[q]
			}
			qf <- as.numeric(t(wal) %*% V %*% wal)
			se[k] <- sqrt(max(0, qf))
			if (is.matrix(ndf)) {
				# Should not occur: matrix df only arises for emmeans engines, which
				# take the branch above. Guard rather than emit an ad-hoc df.
				stop(
					"General contrasts are not supported for this model: it reports ",
					"comparison-specific degrees of freedom but provides no emmeans ",
					"reference grid to derive an exact contrast df.",
					call. = FALSE
				)
			}
			df_c[k] <- ndf
		}
	}

	tstat <- est / se
	p_raw <- 2 * stats::pt(-abs(tstat), df = df_c)
	p_adj <- stats::p.adjust(p_raw, method = adjust)
	crit <- stats::qt(1 - sig / 2, df_c)

	block <- data.frame(
		comparison = names(group_contrasts),
		estimate = est,
		std.error = se,
		statistic = tstat,
		df = df_c,
		p.value = p_adj,
		conf.low = est - crit * se,
		conf.high = est + crit * se,
		stringsAsFactors = FALSE,
		check.names = FALSE
	)

	if (!is.null(by)) {
		by_df <- pp[rep(idx[1], nrow(block)), by, drop = FALSE]
		rownames(by_df) <- NULL
		block <- cbind(by_df, block)
	}

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
	header <- if (identical(attr(x, "comparison_type"), "contrasts")) {
		"Contrasts of means"
	} else {
		"Pairwise comparisons of means"
	}
	cat(header, "\n", sep = "")
	cat("Classify:", attr(x, "classify"), "\n")
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

	note <- aliased_note(attr(x, "aliased"))
	if (!is.null(note)) {
		cat("\n", note, "\n", sep = "")
	}

	invisible(x)
}
