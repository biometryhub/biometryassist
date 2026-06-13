#' Internal model-information extraction for resplot()
#'
#' `extract_model_info()` is the internal generic that [resplot()] uses to pull
#' the residuals, fitted values and (optionally) the model call from a fitted
#' model. It dispatches on the class of `model.obj`. It is not exported and is
#' not called directly by users; support for a new model engine is added by
#' writing a new `extract_model_info()` method.
#'
#' @param model.obj A fitted model object of a supported class (see
#'   *Supported model types* below).
#' @param call Logical; whether to extract the model call for display.
#'
#' @section Supported model types:
#' [resplot()] produces residual diagnostics for any model with an
#' `extract_model_info()` method. These are currently:
#'
#' | Model class | Fitted by | Notes |
#' | --- | --- | --- |
#' | `aov`, `lm` | [stats::aov()], [stats::lm()] | Fixed-effects linear models. |
#' | `aovlist` | [stats::aov()] with an `Error()` term | Multi-stratum aov; each error stratum (except the intercept) is shown as a separate plot. |
#' | `lme` | [nlme::lme()] | Linear mixed model. |
#' | `lmerMod` | [lme4::lmer()] | Linear mixed model. |
#' | `lmerModLmerTest` | [lmerTest::lmer()] | As `lmerMod`. |
#' | `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). Residual strata are shown as separate plots. |
#' | `mmer`, `mmes` | sommer `mmer()` / `mmes()` | Linear mixed model. |
#' | `art` | `ARTool::art()` | Aligned rank transform model. |
#' | `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; a single diagnostic panel from the model residuals. |
#' | `glmmTMB` | glmmTMB `glmmTMB()` | **Gaussian family only.** Non-Gaussian families error with a pointer to `DHARMa::simulateResiduals()`, since a normal Q-Q plot is not a valid diagnostic for them. |
#'
#' This set differs slightly from the comparison functions (see
#' [get_predictions()]): `resplot()` additionally supports sommer and ARTool
#' models.
#'
#' To add a new engine, write an `extract_model_info.<class>()` method returning
#' a list with elements `facet`, `facet_name`, `resids`, `fits`, `k` and
#' `model_call`, and add a row to the table above.
#'
#' @returns A list with elements `facet`, `facet_name`, `resids`, `fits`, `k`
#'   and `model_call`.
#'
#' @seealso [resplot()]
#' @keywords internal
extract_model_info <- function(model.obj, call = FALSE) {
	UseMethod("extract_model_info")
}

#' @noRd
#' @exportS3Method extract_model_info default
extract_model_info.default <- function(model.obj, call = FALSE) {
	supported_types <- c(
		"aov",
		"lm",
		"aovlist",
		"lme",
		"lmerMod",
		"lmerModLmerTest",
		"asreml",
		"mmer",
		"mmes",
		"art",
		"afex_aov",
		"glmmTMB"
	)
	stop(
		"model.obj must be a linear (mixed) model object. Currently supported model types are: ",
		paste(supported_types, collapse = ", "),
		call. = FALSE
	)
}

#' @noRd
#' @exportS3Method extract_model_info aov
extract_model_info.aov <- function(model.obj, call = FALSE) {
	resids <- residuals(model.obj)
	fits <- fitted(model.obj)
	k <- length(resids)

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(model.obj$call, width.cutoff = 50)),
			collapse = "\n"
		)
	}

	output <- list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)

	return(output)
}

#' @noRd
#' @exportS3Method extract_model_info lm
extract_model_info.lm <- extract_model_info.aov

#' @noRd
#' @exportS3Method extract_model_info lme
extract_model_info.lme <- extract_model_info.aov

#' @noRd
#' @exportS3Method extract_model_info aovlist
extract_model_info.aovlist <- function(model.obj, call = FALSE) {
	# An aovlist (aov fitted with an Error() term) is a list of per-stratum aov
	# objects, one per error stratum. The intercept-only stratum carries no useful
	# residual diagnostic, so it is dropped; each remaining stratum becomes a facet,
	# mirroring the multi-stratum handling in extract_model_info.asreml().
	strata <- names(model.obj)
	strata <- strata[strata != "(Intercept)"]

	resids <- unlist(lapply(strata, function(s) residuals(model.obj[[s]])))
	fits <- unlist(lapply(strata, function(s) fitted(model.obj[[s]])))
	k <- vapply(
		strata,
		function(s) length(residuals(model.obj[[s]])),
		numeric(1)
	)
	names(resids) <- NULL
	names(fits) <- NULL
	names(k) <- NULL

	facet <- length(strata)
	facet_name <- if (facet > 1) strata else NULL

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(attr(model.obj, "call"), width.cutoff = 50)),
			collapse = "\n"
		)
	}

	list(
		facet = facet,
		facet_name = facet_name,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)
}

#' @noRd
#' @exportS3Method extract_model_info listof
extract_model_info.listof <- extract_model_info.aovlist

#' @noRd
#' @exportS3Method extract_model_info lmerMod
extract_model_info.lmerMod <- function(model.obj, call = FALSE) {
	resids <- residuals(model.obj)
	fits <- fitted(model.obj)
	k <- length(resids)

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(model.obj@call, width.cutoff = 50)),
			collapse = "\n"
		)
	}

	output <- list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)

	return(output)
}

#' @noRd
#' @exportS3Method extract_model_info lmerModLmerTest
extract_model_info.lmerModLmerTest <- extract_model_info.lmerMod

#' @noRd
#' @exportS3Method extract_model_info asreml
extract_model_info.asreml <- function(model.obj, call = FALSE) {
	facet <- length(names(model.obj$R.param))

	if (facet > 1) {
		facet_name <- names(model.obj$R.param)
		k <- unlist(lapply(1:facet, function(i) {
			model.obj$R.param[[i]]$variance$size
		}))
	} else {
		facet_name <- NULL
		k <- length(model.obj$residual)
	}

	resids <- as.numeric(model.obj[["residuals"]])
	fits <- model.obj[["linear.predictors"]]

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(model.obj$call, width.cutoff = 50)),
			collapse = "\n"
		)
		model_call <- gsub(
			"G\\.param \\= model\\.asr\\$G\\.param, ",
			"",
			model_call
		)
		model_call <- gsub(
			"R\\.param = model\\.asr\\$R\\.param, \\\n",
			"",
			model_call
		)
	}

	output <- list(
		facet = facet,
		facet_name = facet_name,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)

	return(output)
}

#' @noRd
#' @exportS3Method extract_model_info mmer
extract_model_info.mmer <- function(model.obj, call = FALSE) {
	facet <- model.obj$termsN$rcov
	k <- length(model.obj$residual)

	resids <- residuals(model.obj)
	fits <- fitted(model.obj)

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(
				model.obj$call[c("fixed", "random", "rcov")],
				width.cutoff = 50
			)),
			collapse = "\n"
		)
		model_call <- gsub("list", "mmer", model_call)
	}

	output <- list(
		facet = facet,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)

	return(output)
}

#' @noRd
#' @exportS3Method extract_model_info mmes
extract_model_info.mmes <- function(model.obj, call = FALSE) {
	k <- length(model.obj$residual)

	resids <- as.numeric(residuals(model.obj))
	fits <- as.numeric(fitted(model.obj))

	model_call <- NULL
	if (call) {
		model_call <- "Model call not currently available for mmes models."
	}

	list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)
}

#' @noRd
#' @exportS3Method extract_model_info art
extract_model_info.art <- function(model.obj, call = FALSE) {
	resids <- residuals(model.obj)
	k <- length(resids)
	fits <- model.obj$cell.means[, ncol(model.obj$cell.means)]

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(model.obj$call, width.cutoff = 50)),
			collapse = "\n"
		)
	}

	list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)
}

#' @noRd
#' @exportS3Method extract_model_info glmmTMB
extract_model_info.glmmTMB <- function(model.obj, call = FALSE) {
	# resplot() draws Gaussian diagnostics (a normal Q-Q plot), which are only valid
	# for a Gaussian-family fit. For any other family, error and point the user at
	# DHARMa, which provides simulated quantile residuals appropriate for GLMMs.
	fam <- stats::family(model.obj)$family
	if (!identical(fam, "gaussian")) {
		stop(
			"resplot() produces Gaussian residual diagnostics (a normal Q-Q plot), ",
			"which are not valid for a glmmTMB model with family \"",
			fam,
			"\".\n",
			"  Use DHARMa::simulateResiduals() for residual diagnostics of ",
			"non-Gaussian (mixed) models.",
			call. = FALSE
		)
	}

	resids <- as.numeric(residuals(model.obj))
	fits <- as.numeric(fitted(model.obj))
	k <- length(resids)

	model_call <- NULL
	if (call) {
		model_call <- paste(
			trimws(deparse(model.obj$call, width.cutoff = 50)),
			collapse = "\n"
		)
	}

	list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)
}

#' @noRd
#' @exportS3Method extract_model_info afex_aov
extract_model_info.afex_aov <- function(model.obj, call = FALSE) {
	# residuals()/fitted() on an afex_aov emit informational messages about the data
	# being changed during ANOVA calculation; suppress those and return the residual
	# vector from the underlying model. A single diagnostic panel is produced.
	resids <- suppressMessages(as.numeric(residuals(model.obj)))
	fits <- suppressMessages(as.numeric(fitted(model.obj)))
	k <- length(resids)

	model_call <- NULL
	if (call) {
		# afex_aov objects do not store a call; build a representative formula from
		# the response (dv) and the ANOVA-table effects.
		model_call <- paste0(
			attr(model.obj, "dv"),
			" ~ ",
			paste(rownames(model.obj$anova_table), collapse = " + ")
		)
	}

	list(
		facet = 1,
		facet_name = NULL,
		resids = resids,
		fits = fits,
		k = k,
		model_call = model_call
	)
}
