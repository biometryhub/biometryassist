# Helpers shared across the comparison functions multiple_comparisons(),
# pairwise_comparisons() and reference_comparisons(). Helpers specific to a
# single one of these live alongside that function; genuinely package-wide
# utilities live in utils.R.

#' Validate shared inputs for the comparison functions
#'
#' Shared by [multiple_comparisons()], [pairwise_comparisons()] and
#' [reference_comparisons()]. Checks the significance level, rejects reserved
#' `classify` column names, and warns when the response appears to be transformed
#' in the model formula. Returns the individual `classify` variable names.
#'
#' @param sig Significance level.
#' @param classify The `classify` string.
#' @param model.obj The fitted model object.
#' @param trans The transformation argument (or `NULL`).
#' @param trans_supported Logical; `TRUE` for [multiple_comparisons()] (which has
#'   a `trans` argument), `FALSE` for the difference-based functions (which
#'   report on the model scale). Controls the transformed-response warning hint.
#' @return Character vector of `classify` variable names.
#' @importFrom stats formula
#' @keywords internal
#' @noRd
validate_inputs <- function(
	sig,
	classify,
	model.obj,
	trans,
	trans_supported = TRUE
) {
	# Check significance level
	if (sig >= 0.5) {
		if (sig >= 1 & sig < 50) {
			stop(
				"Significance level given by `sig` is high. Perhaps you meant ",
				sig / 100,
				"?",
				call. = FALSE
			)
		} else if (sig >= 1 & sig >= 50) {
			stop(
				"Significance level given by `sig` is high. Perhaps you meant ",
				1 - (sig / 100),
				"?",
				call. = FALSE
			)
		} else {
			warning(
				"Significance level given by `sig` is high. Perhaps you meant ",
				1 - sig,
				"?",
				call. = FALSE
			)
		}
	}

	# Get the individual names provided in classify
	vars <- unlist(strsplit(classify, "\\:"))
	reserved_col_names <- c(
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
	if (any(vars %in% reserved_col_names)) {
		stop(
			"Invalid column name. Please change the name of column(s): ",
			vars[vars %in% reserved_col_names],
			call. = FALSE
		)
	}

	# Check if the response variable is transformed in the model formula
	if (class(model.obj)[1] == c("aovlist")) {
		model_formula <- stats::formula(model.obj[[1]])
	} else {
		model_formula <- stats::formula(model.obj)
	}
	if (inherits(model.obj, "asreml")) {
		response_part <- model_formula[[1]][[2]]
	} else {
		response_part <- model_formula[[2]]
	}
	if (is.call(response_part) & is.null(trans)) {
		hint <- if (trans_supported) {
			"\nPlease specify the 'trans' argument if you want back-transformed predictions."
		} else {
			# pairwise_comparisons()/reference_comparisons() have no `trans`
			# argument; comparisons of differences are reported on the model scale.
			"\nComparisons are reported on the model (transformed) scale."
		}
		warning(
			call. = FALSE,
			sprintf(
				"The response variable appears to be transformed in the model formula: %s.",
				deparse(response_part)
			),
			hint
		)
	}

	return(vars)
}


#' Build per-row treatment labels from one or more factor columns
#'
#' Shared by [multiple_comparisons()] and [pairwise_comparisons()] to turn the
#' classify factor column(s) of a predictions data frame into a single label per
#' row. A single factor is returned as-is (preserving its type); multiple
#' factors (an interaction) are joined with `sep`. The caller chooses the
#' separator (`multiple_comparisons()` uses `"_"`, `pairwise_comparisons()` uses
#' `":"`) and any further processing (e.g. coercion to character).
#'
#' @param pp A predictions data frame.
#' @param vars Character vector of factor column name(s) to combine.
#' @param sep Separator used to join the columns when `vars` has length > 1.
#'
#' @return A vector of labels, one per row of `pp`.
#'
#' @keywords internal
make_treatment_labels <- function(pp, vars, sep) {
	if (length(vars) == 1) {
		pp[[vars]]
	} else {
		apply(pp[, vars, drop = FALSE], 1, paste, collapse = sep)
	}
}


#' Format the aliased-levels note for the comparison print methods
#'
#' Shared by `print.mct()`, `print.pairwise_comparisons()` and
#' `print.reference_comparisons()` so all three report aliasing identically.
#' Returns `NULL` when nothing was aliased. The levels are listed when there are
#' few; once there are more than 6 the list is collapsed to a count (to
#' avoid a large block) and the user is shown how to retrieve them from the
#' `aliased` attribute.
#'
#' @param aliased Character vector of aliased level labels (or `NULL`).
#' @return A single-line character string, or `NULL`.
#' @keywords internal
aliased_note <- function(aliased) {
	aliased <- as.character(aliased)
	n <- length(aliased)
	if (n == 0) {
		return(NULL)
	}
	if (n == 1) {
		return(paste0("Aliased level is: ", aliased))
	}
	if (n > 6) {
		# Too many to list: show how to retrieve them from the output object.
		return(paste0(
			n,
			" levels are aliased. Run attr(x, \"aliased\") to see them."
		))
	}
	paste0(
		"Aliased levels are: ",
		paste(aliased[-n], collapse = ", "),
		" and ",
		aliased[n]
	)
}


#' Note when per-comparison CIs and adjusted p-values can disagree
#'
#' Shared by `pairwise_comparisons()` and `reference_comparisons()`. For
#' non-simultaneous adjustments the confidence intervals are per-comparison (at
#' level `sig`) while the p-values are adjusted for multiplicity, so an interval
#' can exclude zero while the adjusted p-value is not significant (or, more
#' rarely, the reverse). Emits a one-time explanatory `message()` when this
#' actually occurs in the result. Dunnett intervals are the simultaneous
#' intervals and agree with the test by construction, so they are never flagged;
#' `"none"` likewise never disagrees.
#'
#' @param x A comparison table with `conf.low`, `conf.high` and `p.value`.
#' @param sig The significance level used for the intervals and tests.
#' @param method The resolved adjustment method (e.g. `"holm"`, `"dunnett"`).
#' @return Invisibly `TRUE` if a message was shown, otherwise `FALSE`.
#' @keywords internal
note_ci_padjust_mismatch <- function(x, sig, method) {
	# Dunnett intervals are simultaneous and agree with the test by construction.
	if (identical(method, "dunnett")) {
		return(invisible(FALSE))
	}
	if (!all(c("conf.low", "conf.high", "p.value") %in% names(x))) {
		return(invisible(FALSE))
	}
	ci_excludes_zero <- x$conf.low > 0 | x$conf.high < 0
	significant <- x$p.value < sig
	mismatch <- !is.na(ci_excludes_zero) &
		!is.na(significant) &
		(ci_excludes_zero != significant)
	if (any(mismatch)) {
		message(
			"Note: confidence intervals are per-comparison (not adjusted for ",
			"multiplicity), while the p-values are adjusted. A comparison's ",
			"interval can therefore exclude zero when its adjusted p-value is not ",
			"significant at `sig` (or, less often, the reverse)."
		)
		return(invisible(TRUE))
	}
	invisible(FALSE)
}


#' Compute one by-group block of the pairwise comparison table
#'
#' @param group_pairs List of length-2 character vectors (within-group labels).
#' @param idx Global row indices of this group in `pp`.
#' @param group_labels Within-group labels for `idx` (same order as `idx`).
#' @param pp,vcov,ndf Predictions, the prediction variance-covariance matrix and
#'   degrees of freedom from `get_predictions()`. The standard error of each
#'   difference is taken from `vcov` (`sqrt(V_ii + V_jj - 2 V_ij)`).
#' @param adjust,sig,by,descending,include_means As in [pairwise_comparisons()].
#' @noRd
build_pairwise_block <- function(
	group_pairs,
	idx,
	group_labels,
	pp,
	vcov,
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

	# Standard error of each difference from the prediction vcov:
	# Var(mu_i - mu_j) = V_ii + V_jj - 2 V_ij. Floor at zero against rounding.
	vd <- diag(vcov)
	se <- sqrt(pmax(
		0,
		vd[i_idx] + vd[j_idx] - 2 * as.numeric(vcov[cbind(i_idx, j_idx)])
	))
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
		dn <- dunnett_adjust(i_idx, j_idx, se, df_ij, tstat, vcov, sig)
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
#' Uses the prediction variance-covariance matrix `vcov` to form the correlation
#' matrix of the contrasts, and the multivariate-t distribution (`mvtnorm`) for
#' exact simultaneous two-sided adjusted p-values and a single simultaneous
#' critical value for the confidence intervals. Validated to machine precision
#' against `emmeans` `trt.vs.ctrl` with `adjust = "mvt"`.
#'
#' Assumes a single (effectively integer) degrees-of-freedom across the block;
#' callers must fall back to another method when df vary by comparison.
#'
#' @param i_idx,j_idx Global prediction-row indices for each contrast's level1
#'   and level2.
#' @param se,df_ij,tstat Per-contrast SED, degrees of freedom and t-statistic.
#' @param vcov Variance-covariance matrix of the predicted means from
#'   `get_predictions()` (indexed by global prediction row).
#' @param sig Significance level.
#' @noRd
dunnett_adjust <- function(i_idx, j_idx, se, df_ij, tstat, vcov, sig) {
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

	# Sub-covariance over the involved means (taken directly from the model vcov)
	u <- sort(unique(c(i_idx, j_idx)))
	V <- vcov[u, u, drop = FALSE]
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
