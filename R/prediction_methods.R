#' Check if classify term exists in model terms
#'
#' Checks if the classify term exists in the model terms, handling interaction
#' terms specified in any order (e.g., B:A when model has A:B, or A:C:B when
#' model has A:B:C).
#'
#' @param classify Name of predictor variable as a string
#' @param model_terms Character vector of model term labels
#'
#' @return The classify term as it appears in the model (potentially reordered),
#' or throws an error if not found
#' @keywords internal
check_classify_in_terms <- function(classify, model_terms) {
	# First check if classify is directly in model terms
	if (classify %in% model_terms) {
		return(classify)
	}

	# If classify contains ":", it might be an interaction in a different order
	if (grepl(":", classify)) {
		# Split the classify term into parts
		classify_parts <- unlist(strsplit(classify, ":"))

		# For each model term, check if it's the same interaction in a different order
		for (term in model_terms) {
			if (grepl(":", term)) {
				term_parts <- unlist(strsplit(term, ":"))

				# Check if they have the same components (regardless of order)
				# and the same number of components
				if (length(classify_parts) == length(term_parts)) {
					# Sort both sets of parts and compare
					if (all(sort(classify_parts) == sort(term_parts))) {
						# Found a match! Return the model's version
						return(term)
					}
				}
			}
		}
	}

	# If we get here, classify is not in the model in any order
	stop(
		classify,
		" is not a term in the model. Please check model specification.",
		call. = FALSE
	)
}

#' Strip ASReml-R special functions from a term label
#'
#' ASReml-R keeps special-function wrappers in its term labels (e.g.
#' `at(Year):Prior_crop`, `fa(Site, 2):Variety`, `vm(Genotype, Ainv)`), but
#' `predict.asreml()` expects the bare factor names in `classify`
#' (`Year:Prior_crop`). This replaces each whitelisted wrapper with its first
#' argument. Only ASReml-R's own functions are stripped, so base R calls such as
#' `log(x)` are left alone; `random = TRUE` adds the variance-model and
#' relationship-matrix functions, which only have that meaning in the random
#' formula (e.g. `exp()` is a variance model there, but a base function in the
#' fixed formula). Covariate functions (`pol()`, `lin()`, `spl()`, `dev()`,
#' `leg()`) are deliberately not stripped: comparisons on a covariate are not
#' meaningful.
#'
#' @param labels Character vector of term labels.
#' @param random Logical; also strip random-formula-only functions.
#'
#' @return Character vector of labels with the wrappers removed.
#' @keywords internal
strip_asreml_specials <- function(labels, random = FALSE) {
	specials <- "at"
	if (random) {
		specials <- c(specials, asreml_random_specials)
	}

	strip <- function(expr) {
		if (!is.call(expr)) {
			return(expr)
		}
		fn <- as.character(expr[[1]])
		if (identical(fn, ":")) {
			expr[[2]] <- strip(expr[[2]])
			expr[[3]] <- strip(expr[[3]])
			return(expr)
		}
		if (fn %in% specials && length(expr) >= 2) {
			return(expr[[2]])
		}
		expr
	}

	vapply(
		labels,
		function(label) {
			expr <- tryCatch(str2lang(label), error = function(e) NULL)
			if (is.null(expr)) {
				return(label)
			}
			paste(deparse(strip(expr), width.cutoff = 500L), collapse = "")
		},
		character(1),
		USE.NAMES = FALSE
	)
}

# ASReml-R variance-model and relationship-matrix functions, which wrap a factor
# in random terms (e.g. `diag(Site):Variety`, `fa(Site, 2):Variety`,
# `vm(Genotype, Ainv)`). The bare factor is what predict.asreml() classifies on.
asreml_random_specials <- c(
	# relationship matrices
	"vm",
	"ide",
	# identity / diagonal / unstructured / factor analytic
	"id",
	"idv",
	"idh",
	"diag",
	"us",
	"chol",
	"cholc",
	"ante",
	"fa",
	"rr",
	"sfa",
	"facv",
	# correlation models (and their v / h variants)
	paste0(
		rep(
			c(
				"cor",
				"corb",
				"corg",
				"ar1",
				"ar2",
				"ar3",
				"sar",
				"sar2",
				"ma1",
				"ma2",
				"arma",
				"exp",
				"iexp",
				"aexp",
				"gau",
				"igau",
				"agau",
				"mtrn"
			),
			each = 3
		),
		c("", "v", "h")
	)
)

#' Per-pair denominator df for a classify term fitted with at()
#'
#' `wald()` splits an `at(F):X` term into one row per level of `F`
#' (`at(F, 'a'):X`, `at(F, 'b'):X`, ...), each with its own denDF. For
#' `classify = "F:X"`, a comparison between two predictions within one level of
#' `F` uses that level's denDF, which is the df ASReml-R uses to test the `X`
#' effect at that level. A comparison across levels has no exact df (with
#' level-specific residual variances it is a Welch-type problem); the smaller of
#' the two levels' denDF is used as a conservative bound.
#'
#' @param classify The (bare) classify term.
#' @param dendf Data frame with columns `Source` and `denDF` from `wald()`.
#' @param pp Predictions data frame, one row per predicted mean.
#' @param resid_df Residual df, used for levels of `F` without an at() row (when
#'   `at()` was given a subset of levels).
#'
#' @return A square df matrix matching the rows of `pp`, a single value if every
#'   pair has the same df, or `NULL` if `classify` does not match an at() term.
#' @keywords internal
at_term_df <- function(classify, dendf, pp, resid_df) {
	flatten <- function(expr) {
		if (is.call(expr) && identical(as.character(expr[[1]]), ":")) {
			return(c(flatten(expr[[2]]), flatten(expr[[3]])))
		}
		list(expr)
	}
	is_at <- function(expr) {
		is.call(expr) &&
			identical(as.character(expr[[1]]), "at") &&
			length(expr) == 3 &&
			is.atomic(expr[[3]]) &&
			length(expr[[3]]) == 1
	}

	classify_parts <- sort(unlist(strsplit(classify, ":")))
	at_factor <- NULL
	level_df <- numeric(0)

	for (k in seq_len(nrow(dendf))) {
		expr <- tryCatch(
			str2lang(as.character(dendf$Source[k])),
			error = function(e) NULL
		)
		if (is.null(expr)) {
			next
		}
		parts <- flatten(expr)
		at_parts <- vapply(parts, is_at, logical(1))
		if (sum(at_parts) != 1) {
			next
		}
		bare <- vapply(
			parts,
			function(p) deparse(if (is_at(p)) p[[2]] else p),
			character(1)
		)
		if (!identical(sort(bare), classify_parts)) {
			next
		}
		at_call <- parts[[which(at_parts)]]
		factor_name <- deparse(at_call[[2]])
		if (!is.null(at_factor) && factor_name != at_factor) {
			return(NULL)
		}
		at_factor <- factor_name
		level_df[as.character(at_call[[3]])] <- dendf$denDF[k]
	}

	if (is.null(at_factor) || !at_factor %in% names(pp)) {
		return(NULL)
	}
	if (all(is.na(level_df))) {
		return(NULL)
	}

	row_df <- unname(level_df[as.character(pp[[at_factor]])])
	row_df[is.na(row_df)] <- resid_df
	if (length(unique(row_df)) == 1) {
		return(row_df[1])
	}
	outer(row_df, row_df, pmin)
}

#' Get the response label from a model formula
#'
#' @param model.obj A fitted model object with a [stats::formula()] method.
#'
#' @return The left-hand side of the model formula as a string, for use as the
#' plot label.
#' @keywords internal
response_label <- function(model.obj) {
	formula_text <- deparse(stats::formula(model.obj))
	return(trimws(strsplit(formula_text, "~")[[1]][1]))
}

#' Build the SED matrix from a prediction variance-covariance matrix
#'
#' @param vcov Variance-covariance matrix of the predicted means.
#'
#' @return Matrix of standard errors of difference,
#'   `SED_ij = sqrt(V_ii + V_jj - 2 * V_ij)`. The diagonal is left for the
#'   caller to set.
#' @keywords internal
sed_from_vcov <- function(vcov) {
	vd <- diag(vcov)
	sed <- outer(vd, vd, "+") - 2 * vcov
	sed[sed < 0] <- 0 # guard tiny negatives from rounding
	return(sqrt(sed))
}

#' Internal prediction extraction for the comparison functions
#'
#' `get_predictions()` is the internal generic that [multiple_comparisons()],
#' [pairwise_comparisons()] and [reference_comparisons()] use to obtain the
#' predicted means, the standard-error-of-differences (SED) matrix and the
#' degrees of freedom from a fitted model. It dispatches on the class of
#' `model.obj`. It is not exported and is not called directly by users; support
#' for a new model engine is added by writing a new `get_predictions()` method.
#'
#' @param model.obj A fitted model object of a supported class (see
#'   *Supported model types* below).
#' @param classify Name of the predictor variable(s) as a string.
#' @param pred.obj Optional precomputed prediction object (`asreml` only;
#'   otherwise predictions are computed internally).
#' @param ... Additional arguments passed to the class-specific method (e.g.
#'   ASReml-R `predict()` arguments).
#'
#' @section Supported model types:
#' The comparison functions ([multiple_comparisons()], [pairwise_comparisons()]
#' and [reference_comparisons()]) work with any model for which a
#' `get_predictions()` method is defined. These are currently:
#'
#' | Model class | Fitted by | Notes |
#' | --- | --- | --- |
#' | `aov`, `lm` | [stats::aov()], [stats::lm()] | Fixed-effects linear models. |
#' | `aovlist` | [stats::aov()] with an `Error()` term | Multi-stratum aov; degrees of freedom are comparison-specific (a matrix) when comparisons span strata. |
#' | `lme` | [nlme::lme()] | Linear mixed model. |
#' | `lmerMod` | [lme4::lmer()], `lme4breeding::lmebreed()` | Linear mixed model. `lmebreed()` (relationship-based) models also carry class `lmerMod`; comparisons target the fixed-effect means with Kenward-Roger degrees of freedom, and correctly reflect the relationship structure (validated against ASReml-R). |
#' | `lmerModLmerTest` | [lmerTest::lmer()] | As `lmerMod`, with Satterthwaite degrees of freedom. |
#' | `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). |
#' | `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; degrees of freedom are comparison-specific (a matrix) when comparisons span strata. |
#' | `glmmTMB` | glmmTMB `glmmTMB()` | Generalized linear mixed model. Predictions are on the link scale with asymptotic (infinite) degrees of freedom; supply `trans` to back-transform. |
#' | `mmes` | sommer `mmes()` | Linear mixed model, via sommer's native `predict()`. SED from the prediction covariance; asymptotic (infinite) degrees of freedom (sommer provides none). |
#'
#' ARTool (`art`) models are supported by [resplot()] but **not** by the comparison
#' functions: the aligned rank transform makes mean-based comparisons inappropriate.
#' Use `ARTool::art.con()` for contrasts on ART models instead.
#'
#' sommer `mmer` models (the legacy interface) are supported by [resplot()] but
#' **not** by the comparison functions: current sommer provides no `predict()` method
#' for `mmer`. Refit with `sommer::mmes()` to use the comparison functions.
#'
#' To add a new engine, write a `get_predictions.<class>()` method returning a
#' list with elements `predictions`, `sed`, `df`, `ylab`, `aliased_names` and
#' `classify` (plus `emmeans_grid` for engines backed by [emmeans::emmeans()]),
#' and add a row to the table above.
#'
#' @section ASReml-R terms in `classify`:
#' For `asreml` models, `classify` names the factors to predict, as for
#' ASReml-R `predict()`. A term wrapped in an ASReml-R function can be given
#' either by its factors or as written in the model, so for a model with
#' `at(Year):Treatment` both `classify = "Year:Treatment"` and
#' `classify = "at(Year):Treatment"` give the same result. This applies to
#' `at()` and to the variance-structure and relationship functions in the
#' random model (e.g. `diag()`, `us()`, `fa()`, `vm()`), so
#' `fa(Site, 2):Variety` is classified with `"Site:Variety"`.
#'
#' Predictions of terms in the random model include the random effects
#' (BLUPs). Their comparisons use the residual degrees of freedom, with a
#' warning.
#'
#' For an `at()` term, ASReml-R gives a separate Wald test, with its own
#' denominator degrees of freedom, for each level of the conditioning factor.
#' A comparison within a level uses that level's degrees of freedom; a
#' comparison between levels uses the smaller of the two, as a conservative
#' choice. Levels not included in `at()` use the residual degrees of freedom.
#'
#' @section ASReml-R prediction arguments:
#' For `asreml` models, arguments given in `...` are passed to ASReml-R
#' `predict()`. Those most useful for comparisons are:
#'
#' * `present`: average only over the combinations of factor levels that occur
#'   in the data (see below).
#' * `average`: choose the factors to average over, optionally with weights
#'   (e.g. in proportion to replication rather than equally).
#' * `levels`: predict at chosen levels only, e.g. a subset of treatments, or
#'   at given values of a covariate rather than its mean.
#' * `ignore`, `use`, `except` and `only`: change which model terms enter the
#'   predictions, e.g. `use` to include a random term that is otherwise left
#'   out.
#' * `associate`: declare nested factors, e.g. treatments nested within
#'   treatment types.
#'
#' `classify`, `sed` and `vcov` are set by the comparison functions and cannot
#' be passed. `aliased = TRUE` (predictions of non-estimable functions) and
#' `evaluate = FALSE` are not suitable. See ASReml-R `?predict.asreml` for full
#' details of each argument.
#'
#' **When to use `present`.** By default ASReml-R `predict()` averages over
#' every combination of the levels of the fixed factors not in `classify`. If
#' some combinations were never observed (treatments that differ between sites
#' or years, a control outside a factorial set, or a factor fitted only within
#' some levels of another, as with `at()`), the predictions that need them
#' cannot be estimated: those levels are dropped as aliased, with a warning, or
#' the function stops with "All predicted values are aliased". `present`
#' restricts the averaging to the combinations in the data. Give it the
#' factors involved, usually those in `classify` and those averaged over:
#'
#' ```r
#' multiple_comparisons(model.asr, classify = "Year:Prior_crop",
#'                      present = c("Year", "Prior_crop", "Treatment"))
#' ```
#'
#' Each mean is then averaged over only the combinations observed for it, so
#' two means can rest on different sets of levels of the other factors. Check
#' that this is a fair basis for the comparison.
#'
#' @returns A list with elements `predictions`, `sed`, `df`, `ylab`,
#'   `aliased_names` and `classify` (and `emmeans_grid` for emmeans-backed
#'   engines). `classify` is the input resolved to the factor names the
#'   predictions are labelled by, in the order given by the user (e.g. ASReml-R
#'   wrappers removed: `"at(Year):Prior_crop"` becomes `"Year:Prior_crop"`).
#'   The comparison functions take their `classify` variables from it.
#'
#' @seealso [multiple_comparisons()], [pairwise_comparisons()],
#'   [reference_comparisons()]
#' @keywords internal
get_predictions <- function(model.obj, classify, pred.obj = NULL, ...) {
	UseMethod("get_predictions")
}

#' @noRd
#' @exportS3Method get_predictions default
get_predictions.default <- function(model.obj, ...) {
	supported_types <- c(
		"aov",
		"lm",
		"aovlist",
		"lmerMod",
		"lmerModLmerTest",
		"lme",
		"asreml",
		"afex_aov",
		"glmmTMB",
		"mmes"
	)
	stop(
		"model.obj must be a linear (mixed) model object. Currently supported model types are: ",
		paste(supported_types, collapse = ", "),
		call. = FALSE
	)
}

#' @noRd
#' @exportS3Method get_predictions asreml
get_predictions.asreml <- function(model.obj, classify, pred.obj = NULL, ...) {
	args <- list(...)
	# asr_args <- args[names(args) %in% names(formals(asreml::predict.asreml))]
	# Check if classify is in model terms (handles reversed interaction order).
	# ASReml-R special functions are stripped from the term labels and from
	# classify, since predict.asreml() classifies on the bare factor names: both
	# "at(Year):Prior_crop" and "Year:Prior_crop" resolve to "Year:Prior_crop".
	model_terms <- unique(c(
		strip_asreml_specials(
			attr(stats::terms(model.obj$formulae$fixed), 'term.labels')
		),
		strip_asreml_specials(
			attr(stats::terms(model.obj$formulae$random), 'term.labels'),
			random = TRUE
		)
	))
	# Returned to the caller in the user's order (it sets the order of the
	# treatment labels); only the model's ordering is used for prediction.
	classify_label <- strip_asreml_specials(classify, random = TRUE)
	classify <- check_classify_in_terms(classify_label, model_terms)

	# Generate predictions if not provided. `vcov = TRUE` returns the exact
	# variance-covariance of the predicted means, used directly by
	# pairwise_comparisons()/reference_comparisons() for contrasts and the
	# Dunnett correlation (no reconstruction from SEDs needed).
	if (missing(pred.obj) || is.null(pred.obj)) {
		pred.obj <- quiet(asreml::predict.asreml(
			object = model.obj,
			classify = classify,
			sed = TRUE,
			vcov = TRUE,
			trace = FALSE,
			...
		))
	}

	# Check if all predicted values are NA
	if (
		all(is.na(pred.obj$pvals$predicted.value)) &
			all(is.na(pred.obj$pvals$std.error))
	) {
		stop(
			"All predicted values are aliased. Perhaps you need the `present` argument?",
			call. = FALSE
		)
	}

	# For use with asreml 4+
	pp <- pred.obj$pvals
	sed <- pred.obj$sed
	# Exact prediction vcov (NULL on the deprecated `pred.obj` path if it was
	# generated without `vcov = TRUE`; only multiple_comparisons() uses that path,
	# and it relies on `sed`, not `vcov`).
	vcov <- if (!is.null(pred.obj$vcov)) as.matrix(pred.obj$vcov) else NULL

	# Process aliased treatments with asreml-specific exclude columns
	aliased_result <- process_aliased(
		pp,
		sed,
		classify,
		exclude_cols = c("predicted.value", "std.error", "status"),
		vcov = vcov
	)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	vcov <- aliased_result$vcov
	aliased_names <- aliased_result$aliased_names

	# Remove status column if present
	pp$status <- NULL
	# predict.asreml() keeps every level of the classify factors even when
	# `levels` restricts the predictions to a subset; drop the unused ones so
	# the factors match the rows (the letter groupings fail otherwise).
	pp <- droplevels(pp)

	if (!"dendf" %in% names(args)) {
		dat.ww <- quiet(
			asreml::wald(
				model.obj,
				ssType = "conditional",
				denDF = "default",
				trace = FALSE
			)$Wald
		)
		dendf <- data.frame(Source = row.names(dat.ww), denDF = dat.ww$denDF)
	} else {
		dendf <- args$dendf
	}

	ndf <- dendf$denDF[
		grepl(classify, dendf$Source) &
			nchar(classify) == nchar(as.character(dendf$Source))
	]
	# An at() term has no single wald() row: it is split into one row per level
	# of the conditioning factor, each with its own denDF. Use those per pair.
	if (rlang::is_empty(ndf) || all(is.na(ndf))) {
		ndf <- at_term_df(classify, dendf, pp, model.obj$nedf)
	}
	# Fall back to the residual df when wald() returns no usable denominator df
	# for the term (term absent, or an NA denDF).
	if (is.null(ndf) || rlang::is_empty(ndf) || all(is.na(ndf))) {
		ndf <- model.obj$nedf
		warning(
			classify,
			" is not a fixed term in the model. The denominator degrees of freedom are estimated using the residual degrees of freedom. This may be inaccurate.",
			call. = FALSE
		)
	}

	# Get response variable for plot label
	ylab <- model.obj$formulae$fixed[[2]]
	# ylab <- trimws(gsub("\\(|\\)", "", ylab))

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		vcov = vcov,
		classify = classify_label
	))
}

#' @noRd
#' @exportS3Method get_predictions lm
#' @importFrom emmeans emmeans
get_predictions.lm <- function(model.obj, classify, ...) {
	# Check if classify is in model terms (handles reversed interaction order)
	model_terms <- attr(stats::terms(model.obj), 'term.labels')
	classify_label <- classify
	classify <- check_classify_in_terms(classify, model_terms)

	# Set emmeans options
	on.exit(options(emmeans = emmeans::emm_defaults))
	emmeans::emm_options("msg.interaction" = FALSE, "msg.nesting" = FALSE)

	# Generate predictions (keep the reference grid for exact contrast df later)
	emm <- emmeans::emmeans(model.obj, as.formula(paste("~", classify)))

	# Exact variance-covariance of the predicted means, ordered as the grid.
	# Used directly by pairwise_comparisons()/reference_comparisons(), and to
	# build the SED matrix below.
	vcov <- as.matrix(stats::vcov(emm))

	# SED matrix from the prediction vcov. Exact for all designs, including
	# unbalanced marginal means. The earlier sigma * sqrt(1/w_i + 1/w_j) form was
	# only exact for balanced or one-way predictions and was wrong when averaging
	# over an unbalanced factor.
	sed <- sed_from_vcov(vcov)

	pred.out <- as.data.frame(emm)
	pred.out <- pred.out[, !grepl("CL", names(pred.out))]

	# Rename columns for consistency
	pp <- pred.out
	names(pp)[names(pp) == "emmean"] <- "predicted.value"
	names(pp)[names(pp) == "SE"] <- "std.error"

	# Set diagonals to NA
	if (all(dim(sed) > 1)) {
		diag(sed) <- NA
	}

	# Process aliased treatments
	aliased_result <- process_aliased(pp, sed, classify, vcov = vcov)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	vcov <- aliased_result$vcov
	aliased_names <- aliased_result$aliased_names

	# Get degrees of freedom
	ndf <- pp$df[1]

	# Get response variable for plot label
	ylab <- response_label(model.obj)

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		emmeans_grid = emm,
		vcov = vcov,
		classify = classify_label
	))
}


#' Build predictions, SED and df from an emmeans-backed model
#'
#' Shared core for the emmeans-backed `get_predictions()` methods (`aovlist`,
#' `afex_aov`, ...). Given the emmeans reference grid for `classify`, it builds the
#' predicted means, the SED matrix and the degrees of freedom from the pairwise
#' contrasts, and processes aliased levels. The df is a single value when every
#' comparison shares it, and a comparison-specific matrix otherwise.
#'
#' @param model.obj A fitted model object with an `emmeans::emmeans()` method.
#' @param classify Name of the predictor variable(s) as a string.
#' @param model_terms Character vector of model term labels (for the classify
#'   check). Defaults to the term labels of `model.obj`.
#' @param ylab Response variable label for the plot. Defaults to the left-hand
#'   side of the model formula.
#'
#' @return A list with elements `predictions`, `sed`, `df`, `ylab`,
#'   `aliased_names`, `emmeans_grid`, `vcov` and `classify`.
#' @keywords internal
predictions_from_emmeans <- function(
	model.obj,
	classify,
	model_terms = attr(stats::terms(model.obj), 'term.labels'),
	ylab = response_label(model.obj)
) {
	# Check if classify is in model terms (handles reversed interaction order)
	classify_label <- classify
	classify <- check_classify_in_terms(classify, model_terms)

	# Set emmeans options
	on.exit(options(emmeans = emmeans::emm_defaults))
	emmeans::emm_options("msg.interaction" = FALSE, "msg.nesting" = FALSE)

	# Generate predictions (keep the reference grid for exact contrast df later)
	emm <- emmeans::emmeans(
		model.obj,
		as.formula(paste("~", classify)),
		method = "pairwise"
	)

	# Use emmeans embedded function for multiple comparisons
	aov_compare <- as.data.frame(emmeans::contrast(emm, method = "pairwise"))

	# Convert emmeans predictions to a data frame
	pred.out <- as.data.frame(emm)

	# Extract standard errors
	# define SED matrix (vectorised fill of upper triangle then mirror)
	n <- nrow(pred.out)
	sed <- matrix(NA_real_, nrow = n, ncol = n)
	# obtain residual degrees of freedom matrix
	ndf <- matrix(NA_real_, nrow = n, ncol = n)
	if (n > 1) {
		# emmeans orders pairwise contrasts row-wise (1-2, 1-3, ..., 2-3, ...),
		# but upper.tri() indexes column-wise, so reorder the indices to match
		upper_idx <- which(upper.tri(sed), arr.ind = TRUE)
		upper_idx <- upper_idx[
			order(upper_idx[, 1], upper_idx[, 2]),
			,
			drop = FALSE
		]
		sed[upper_idx] <- aov_compare$SE
		ndf[upper_idx] <- aov_compare$df
		sed[lower.tri(sed)] <- t(sed)[lower.tri(sed)]
		ndf[lower.tri(ndf)] <- t(ndf)[lower.tri(ndf)]
	}

	# Remove columns with upper and lower confidence intervals
	pred.out <- pred.out[, !grepl("CL", names(pred.out))]
	# Remove columns with degrees of freedom
	pred.out <- pred.out[, !grepl("df", names(pred.out))]

	# Rename columns for consistency
	pp <- pred.out
	names(pp)[names(pp) == "emmean"] <- "predicted.value"
	names(pp)[names(pp) == "SE"] <- "std.error"

	# Exact variance-covariance of the predicted means, ordered as the grid.
	# Used directly by pairwise_comparisons()/reference_comparisons().
	vcov <- as.matrix(stats::vcov(emm))

	# Process aliased treatments
	aliased_result <- process_aliased(pp, sed, classify, vcov = vcov, ndf = ndf)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	vcov <- aliased_result$vcov
	ndf <- aliased_result$ndf
	aliased_names <- aliased_result$aliased_names

	# When every comparison shares the same df (e.g. a single error stratum),
	# return it as a single df so callers can use methods that need one, such as
	# the exact Dunnett test.
	df_values <- ndf[!is.na(ndf)]
	if (
		length(df_values) > 0 && isTRUE(all.equal(min(df_values), max(df_values)))
	) {
		ndf <- df_values[1]
	}

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		emmeans_grid = emm,
		vcov = vcov,
		classify = classify_label
	))
}

#' @noRd
#' @exportS3Method get_predictions aovlist
#' @importFrom emmeans emmeans
get_predictions.aovlist <- function(model.obj, classify, ...) {
	# The response label comes from the first stratum's formula
	return(predictions_from_emmeans(
		model.obj,
		classify,
		ylab = response_label(model.obj[[1]])
	))
}

#' @noRd
#' @exportS3Method get_predictions afex_aov
#' @importFrom emmeans emmeans
get_predictions.afex_aov <- function(model.obj, classify, ...) {
	# afex_aov has no terms() method; the model effects are the rows of the ANOVA
	# table and the response is stored in the "dv" attribute. emmeans() dispatches
	# directly on afex_aov objects, so the shared emmeans core does the rest.
	model_terms <- rownames(model.obj$anova_table)
	ylab <- attr(model.obj, "dv")

	return(predictions_from_emmeans(model.obj, classify, model_terms, ylab))
}

#' @noRd
#' @exportS3Method get_predictions glmmTMB
#' @importFrom emmeans emmeans
get_predictions.glmmTMB <- function(model.obj, classify, ...) {
	# emmeans() supports glmmTMB natively (conditional component, link scale by
	# default), and the pairwise-contrast SEs in the shared core give the correct SED
	# from the full coefficient covariance. Degrees of freedom are asymptotic (Inf).
	# For non-Gaussian families predictions are on the link scale; supply `trans` to
	# multiple_comparisons() to back-transform.
	return(predictions_from_emmeans(model.obj, classify))
}

#' @noRd
#' @exportS3Method get_predictions mmes
get_predictions.mmes <- function(model.obj, classify, ...) {
	# sommer has no emmeans support, so use its native predict() with D = classify
	# to obtain predicted means and their covariance matrix. The pairwise SED is then
	# built from that covariance. Fixed model terms come from the Dtable, and the
	# response (ylab) from the stored fixed formula.
	model_terms <- model.obj$Dtable$term[model.obj$Dtable$type == "fixed"]
	model_terms <- setdiff(model_terms, c("1", "(Intercept)"))
	classify_label <- classify
	classify <- check_classify_in_terms(classify, model_terms)

	pred <- predict(model.obj, D = classify)
	pp <- pred$pvals

	# Build the SED matrix from the prediction covariance
	vcov <- as.matrix(pred$vcov)
	sed <- sed_from_vcov(vcov)
	diag(sed) <- NA

	# Process aliased treatments (levels with NA predictions), reusing shared helper.
	aliased_result <- process_aliased(pp, sed, classify)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	aliased_names <- aliased_result$aliased_names

	# sommer provides no denominator degrees of freedom; use asymptotic (z-based)
	# inference, as for glmmTMB.
	ndf <- Inf

	ylab <- model.obj$args$fixed[[2]]

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		emmeans_grid = NULL,
		classify = classify_label
	))
}

#' @noRd
#' @exportS3Method get_predictions mmer
get_predictions.mmer <- function(model.obj, classify, ...) {
	# sommer's legacy `mmer` interface has no predict() method in current sommer, so
	# mean-based comparisons are not available. Point users at the mmes() interface.
	# resplot() still supports `mmer` models.
	stop(
		"sommer `mmer` models are not supported for multiple comparisons ",
		"(sommer no longer provides a predict() method for the legacy `mmer` ",
		"interface).\n",
		"  Refit the model with sommer::mmes() to use the comparison functions.\n",
		"  (`mmer` models are still supported by resplot().)",
		call. = FALSE
	)
}

#' @noRd
#' @exportS3Method get_predictions listof
get_predictions.listof <- function(model.obj, classify, ...) {
	return(get_predictions.aovlist(model.obj, classify, ...))
}


#' @noRd
#' @exportS3Method get_predictions lmerMod
get_predictions.lmerMod <- function(model.obj, classify, ...) {
	return(predictions_from_emmeans(model.obj, classify))
}

#' @noRd
#' @exportS3Method get_predictions lmerModLmerTest
get_predictions.lmerModLmerTest <- function(model.obj, classify, ...) {
	return(get_predictions.lmerMod(model.obj, classify, ...))
}

#' @noRd
#' @exportS3Method get_predictions lme
get_predictions.lme <- function(model.obj, classify, ...) {
	# Use the shared emmeans core rather than the lm method: comparisons need
	# the df of each pairwise contrast, which for lme differs from the df of the
	# individual means.
	return(predictions_from_emmeans(model.obj, classify))
}

#' @noRd
#' @exportS3Method get_predictions art
get_predictions.art <- function(model.obj, classify, ...) {
	# ARTool models are deliberately unsupported for mean-based comparisons: the
	# aligned-rank transform means Tukey-style contrasts on the predicted means are
	# not statistically appropriate. resplot() still supports `art` models.
	stop(
		"ARTool (`art`) models use an aligned rank transform, so mean-based ",
		"multiple comparisons are not appropriate.\n",
		"  Use `ARTool::art.con()` for contrasts on ART models.\n",
		"  (`art` models are still supported by resplot().)",
		call. = FALSE
	)
}

#' Process aliased treatments in predictions
#'
#' @param pp Data frame of predictions
#' @param sed Standard error of differences matrix
#' @param classify Name of predictor variable
#' @param exclude_cols Column names to exclude when processing aliased names
#' @param vcov Optional variance-covariance matrix of the predictions, subset to
#'   the estimable rows/columns alongside `sed` when supplied (`NULL` otherwise).
#' @param ndf Optional degrees of freedom. A comparison-specific (matrix) df is
#'   subset alongside `sed`; a single df is returned unchanged.
#'
#' @return List containing processed predictions, sed matrix, aliased names and
#'   (when supplied) the subset `vcov` and `ndf`.
#' @keywords internal
process_aliased <- function(
	pp,
	sed,
	classify,
	exclude_cols = c("predicted.value", "std.error", "df", "Names"),
	vcov = NULL,
	ndf = NULL
) {
	aliased_names <- NULL

	if (anyNA(pp$predicted.value)) {
		aliased <- which(is.na(pp$predicted.value))
		# Get aliased treatment levels
		aliased_names <- pp[aliased, !names(pp) %in% exclude_cols]

		# Convert to character vector
		if (is.data.frame(aliased_names)) {
			aliased_names <- apply(aliased_names, 1, paste, collapse = ":")
		}

		# Create warning message. Listed when few; collapsed to a count once there
		# are more than 6, to avoid a very large warning block.
		if (length(aliased_names) == 1) {
			warn_string <- paste0(
				"A level of ",
				classify,
				" is aliased. It has been removed from predicted output.\n",
				"  Aliased level is: ",
				aliased_names,
				".\n  This level is saved as an attribute of the output object."
			)
		} else if (length(aliased_names) <= 6) {
			# cap the listing at 6 to avoid a very large warning block
			warn_string <- paste0(
				"Some levels of ",
				classify,
				" are aliased. They have been removed from predicted output.\n",
				"  Aliased levels are: ",
				paste(aliased_names, collapse = ", "),
				".\n  These levels are saved in the output object."
			)
		} else {
			warn_string <- paste0(
				"Some levels of ",
				classify,
				" are aliased (",
				length(aliased_names),
				" levels). They have been removed from predicted output and saved ",
				"in the \"aliased\" attribute of the output object."
			)
		}

		# Remove aliased values
		pp <- pp[!is.na(pp$predicted.value), ]
		pp <- droplevels(pp)
		sed <- sed[-aliased, -aliased]
		if (!is.null(vcov)) {
			vcov <- vcov[-aliased, -aliased, drop = FALSE]
		}
		if (is.matrix(ndf)) {
			ndf <- ndf[-aliased, -aliased, drop = FALSE]
		}
		warning(warn_string, call. = FALSE)
	}

	return(list(
		predictions = pp,
		sed = sed,
		aliased_names = aliased_names,
		vcov = vcov,
		ndf = ndf
	))
}
