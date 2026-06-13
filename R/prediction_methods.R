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
#' | `aovlist` | [stats::aov()] with an `Error()` term | Multi-stratum aov; gives comparison-specific (matrix) degrees of freedom. |
#' | `lme` | [nlme::lme()] | Linear mixed model. |
#' | `lmerMod` | [lme4::lmer()], `lme4breeding::lmebreed()` | Linear mixed model. `lmebreed()` (relationship-based) models also carry class `lmerMod`; comparisons target the fixed-effect means with Kenward-Roger degrees of freedom, and correctly reflect the relationship structure (validated against ASReml-R). |
#' | `lmerModLmerTest` | [lmerTest::lmer()] | As `lmerMod`, with Satterthwaite degrees of freedom. |
#' | `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). |
#' | `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; gives comparison-specific (matrix) degrees of freedom. |
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
#' list with elements `predictions`, `sed`, `df`, `ylab` and `aliased_names`
#' (plus `emmeans_grid` for engines backed by [emmeans::emmeans()]), and add a
#' row to the table above.
#'
#' @returns A list with elements `predictions`, `sed`, `df`, `ylab` and
#'   `aliased_names` (and `emmeans_grid` for emmeans-backed engines).
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
	# Check if classify is in model terms (handles reversed interaction order)
	model_terms <- c(
		attr(stats::terms(model.obj$formulae$fixed), 'term.labels'),
		attr(stats::terms(model.obj$formulae$random), 'term.labels')
	)
	classify <- check_classify_in_terms(classify, model_terms)

	# Generate predictions if not provided
	if (missing(pred.obj) || is.null(pred.obj)) {
		pred.obj <- quiet(asreml::predict.asreml(
			object = model.obj,
			classify = classify,
			sed = TRUE,
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

	# Process aliased treatments with asreml-specific exclude columns
	aliased_result <- process_aliased(
		pp,
		sed,
		classify,
		exclude_cols = c("predicted.value", "std.error", "status")
	)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	aliased_names <- aliased_result$aliased_names

	# Remove status column if present
	pp$status <- NULL

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

	vars <- unlist(strsplit(classify, "\\:"))
	ndf <- dendf$denDF[
		grepl(classify, dendf$Source) &
			nchar(classify) == nchar(as.character(dendf$Source))
	]
	if (rlang::is_empty(ndf)) {
		ndf <- model.obj$nedf
		rand_terms <- vars[
			vars %in%
				attr(stats::terms(model.obj$formulae$random), 'term.labels')
		]
		warning(
			rand_terms,
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
		aliased_names = aliased_names
	))
}

#' @noRd
#' @exportS3Method get_predictions lm
#' @importFrom emmeans emmeans
get_predictions.lm <- function(model.obj, classify, ...) {
	# Check if classify is in model terms (handles reversed interaction order)
	model_terms <- attr(stats::terms(model.obj), 'term.labels')
	classify <- check_classify_in_terms(classify, model_terms)

	# Set emmeans options
	on.exit(options(emmeans = emmeans::emm_defaults))
	emmeans::emm_options("msg.interaction" = FALSE, "msg.nesting" = FALSE)

	# Generate predictions (keep the reference grid for exact contrast df later)
	emm <- emmeans::emmeans(model.obj, as.formula(paste("~", classify)))

	# Extract standard errors and predictions
	sed <- emm@misc$sigma *
		sqrt(outer(1 / emm@grid$.wgt., 1 / emm@grid$.wgt., "+"))
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
	aliased_result <- process_aliased(pp, sed, classify)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	aliased_names <- aliased_result$aliased_names

	# Get degrees of freedom
	ndf <- pp$df[1]

	# Get response variable for plot label
	formula_text <- deparse(stats::formula(model.obj))
	ylab <- strsplit(formula_text, "~")[[1]][1]
	ylab <- trimws(ylab)

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		emmeans_grid = emm
	))
}


#' Build predictions, SED and df from an emmeans-backed model
#'
#' Shared core for the emmeans-backed `get_predictions()` methods (`aovlist`,
#' `afex_aov`, ...). Given the emmeans reference grid for `classify`, it builds the
#' predicted means, the comparison-specific (matrix) SED and degrees of freedom from
#' the pairwise contrasts, and processes aliased levels. The terms check and `ylab`
#' are computed by the caller (these differ per engine) and passed in.
#'
#' @param model.obj A fitted model object with an `emmeans::emmeans()` method.
#' @param classify Name of the predictor variable(s) as a string.
#' @param model_terms Character vector of model term labels (for the classify check).
#' @param ylab Response variable label for the plot.
#'
#' @return A list with elements `predictions`, `sed`, `df`, `ylab`, `aliased_names`
#'   and `emmeans_grid`.
#' @keywords internal
predictions_from_emmeans <- function(model.obj, classify, model_terms, ylab) {
	# Check if classify is in model terms (handles reversed interaction order)
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
		upper_idx <- upper.tri(sed)
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

	# Process aliased treatments
	aliased_result <- process_aliased(pp, sed, classify)
	pp <- aliased_result$predictions
	sed <- aliased_result$sed
	aliased_names <- aliased_result$aliased_names

	return(list(
		predictions = pp,
		sed = sed,
		df = ndf,
		ylab = ylab,
		aliased_names = aliased_names,
		emmeans_grid = emm
	))
}

#' @noRd
#' @exportS3Method get_predictions aovlist
#' @importFrom emmeans emmeans
get_predictions.aovlist <- function(model.obj, classify, ...) {
	model_terms <- attr(stats::terms(model.obj), 'term.labels')

	# Get response variable for plot label
	if (class(model.obj)[1] %in% c("lmerMod", "lmerModLmerTest")) {
		formula_text <- deparse(stats::formula(model.obj))
	} else {
		formula_text <- deparse(stats::formula(model.obj[[1]]))
	}
	ylab <- trimws(strsplit(formula_text, "~")[[1]][1])

	predictions_from_emmeans(model.obj, classify, model_terms, ylab)
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

	predictions_from_emmeans(model.obj, classify, model_terms, ylab)
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
	model_terms <- attr(stats::terms(model.obj), 'term.labels')
	formula_text <- deparse(stats::formula(model.obj))
	ylab <- trimws(strsplit(formula_text, "~")[[1]][1])

	predictions_from_emmeans(model.obj, classify, model_terms, ylab)
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
	classify <- check_classify_in_terms(classify, model_terms)

	pred <- predict(model.obj, D = classify)
	pp <- pred$pvals

	# Build the SED matrix from the prediction covariance:
	# SED_ij = sqrt(V_ii + V_jj - 2 * V_ij).
	vcov <- as.matrix(pred$vcov)
	sed <- sqrt(outer(diag(vcov), diag(vcov), "+") - 2 * vcov)
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
		emmeans_grid = NULL
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
	get_predictions.aovlist(model.obj, classify, ...)
}


#' @noRd
#' @exportS3Method get_predictions lmerMod
get_predictions.lmerMod <- function(model.obj, classify, ...) {
	# Reuse lm method for common functionality
	#result <- get_predictions.lm(model.obj, classify, ...)

	result <- get_predictions.aovlist(model.obj, classify, ...)

	# Override ylab extraction for lmerMod
	# result$ylab <- model.obj@call[[2]][[2]]

	return(result)
}

#' @noRd
#' @exportS3Method get_predictions lmerModLmerTest
get_predictions.lmerModLmerTest <- function(model.obj, classify, ...) {
	get_predictions.lmerMod(model.obj, classify, ...)
}

#' @noRd
#' @exportS3Method get_predictions lme
get_predictions.lme <- function(model.obj, classify, ...) {
	get_predictions.lm(model.obj, classify, ...)
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
#'
#' @return List containing processed predictions, sed matrix and aliased names
#' @keywords internal
process_aliased <- function(
	pp,
	sed,
	classify,
	exclude_cols = c("predicted.value", "std.error", "df", "Names")
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
		warning(warn_string, call. = FALSE)
	}

	return(list(
		predictions = pp,
		sed = sed,
		aliased_names = aliased_names
	))
}
