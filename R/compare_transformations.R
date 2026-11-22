#' Compare transformations of the response of an ASReml-R model
#'
#' Refits an `asreml` model under a set of response transformations and
#' compares their REML log-likelihoods, adjusted so they are comparable across
#' transformations. Optionally profiles the log-likelihood over a grid of
#' Box-Cox powers. This gives an objective comparison of transformations (log,
#' square root, logit, ...) in place of refitting each candidate by hand and
#' inspecting residual plots.
#'
#' @param model.obj A fitted `asreml` model. Its call is re-evaluated with the
#'   response replaced, so the data and any other objects referenced in the
#'   call must be findable from `env`.
#' @param trans The transformations to compare: a character vector, or a list
#'   whose elements are any of:
#'   * `"default"`: the standard set, every transformation supported by
#'     [multiple_comparisons()]: `"none"`, `"sqrt"`, `"log"`, `"inverse"`,
#'     `"logit"` and `"arcsin"`. Members of the standard set that do not apply
#'     to the response (e.g. `"logit"` and `"arcsin"` when it is not a
#'     proportion) are skipped and listed when printed.
#'   * a transformation name from the standard set. Named transformations are
#'     always reported, with a note where they do not apply to the response.
#'   * a single number, giving a power transformation, e.g. `1/3` for a cube
#'     root;
#'   * a named function of the response, for any other monotone, differentiable
#'     transformation, e.g. `list(log10 = function(y) log10(y + 1))`. Custom
#'     functions are applied to the raw response, so include any offset in the
#'     function.
#'
#'   For example, `c("default", 1/3)` adds a cube root to the standard set, and
#'   `c("none", "sqrt", "log")` compares only those three. `"none"` is always
#'   included as the baseline. Default is `"default"`.
#' @param power Optional numeric vector of at least 3 Box-Cox powers
#'   (\eqn{\lambda}) at which to profile the log-likelihood, e.g.
#'   `seq(-1, 2, by = 0.25)`. Default `NULL` skips the profile.
#' @param offset Numeric constant added to the response before power
#'   transformations (including `"sqrt"`, `"log"` and `"inverse"`). Default
#'   `NULL` chooses one automatically: `0` if all responses are positive,
#'   otherwise a shift placing the smallest response at half the smallest
#'   positive gap above it. Not applied to `"logit"`, `"arcsin"` or custom
#'   functions.
#' @param conf_level Confidence level used for the Box-Cox interval and for
#'   judging which transformations are distinguishable. Default is `0.95`.
#' @param max_refits Maximum number of times to restart a non-converged fit
#'   from its own final parameter values. Default is `5`.
#' @param env Environment in which to evaluate the model call. Default is the
#'   calling environment.
#'
#' @details
#' Log-likelihoods of models fitted to differently transformed responses are
#' not comparable as they stand. Under maximum likelihood, adding the
#' log-Jacobian \eqn{\sum \log|g'(y_i)|}{sum(log|g'(y)|)} of the transformation
#' \eqn{g} fixes this, but the REML log-likelihood is the density of error
#' contrasts rather than of \eqn{y}, and the same adjustment is out by
#' \eqn{p \log c}{p * log(c)} even for a rescaling \eqn{z = cy} (\eqn{p} fixed
#' effects). Instead, each model is fitted to the *normalised* transformation
#' \deqn{z = g(y) / \exp\{\overline{\log|g'(y)|}\},}{z = g(y) / exp(mean(log|g'(y)|)),}
#' whose derivative has geometric mean 1. The fitted log-likelihoods are then
#' directly comparable, under both ML and REML. For power transformations this
#' is the normalised Box-Cox transformation (Box and Cox, 1964), extended to
#' linear mixed models by Gurka et al. (2006); the same normalisation applies
#' to any monotone transformation. Derivatives are analytic for the named
#' transformations and numerical for custom functions.
#'
#' Transformations whose log-likelihoods differ by less than
#' \eqn{\chi^2_{1, \alpha} / 2} (about 1.92 at 95%) are not clearly
#' distinguishable. Prefer the simplest such transformation: `suggested` is
#' `"none"` if it is within that distance of the best, and the best
#' transformation otherwise. This is a heuristic for non-nested candidates.
#'
#' For the Box-Cox profile, the approximate confidence interval contains all
#' \eqn{\lambda} with log-likelihood within the same distance of the maximum,
#' with the end points found by linear interpolation between grid values.
#' Rather than the exact maximum, choose an interpretable power inside the
#' interval: 1 (no transformation) if it is included, otherwise e.g. 0.5
#' (square root), 0 (log) or -1 (inverse).
#'
#' The comparison is only valid if every fit uses the same fixed, random and
#' residual structures and the same observations. Each transformation is
#' fitted as the original model was: if its call supplied `G.param`/`R.param`
#' (e.g. from `update()`, or to fix or constrain parameters), the model
#' object's own parameter tables are used, keeping any constraints; otherwise
#' default starting values are used. The untransformed fit is compared with the
#' original model's log-likelihood as a check that the data have not changed.
#'
#' @section Caveats:
#' * The offset is effectively a second transformation parameter. Results,
#'   particularly for log and negative powers, depend on it, but comparisons
#'   remain valid because it is held fixed.
#' * With many zeros, results for log-like transformations are driven by how
#'   the zeros are shifted. A GLMM or a two-part model may be more
#'   appropriate; GLMM likelihoods are not comparable with these Gaussian fits.
#' * `"logit"` and `"arcsin"` need proportions strictly between 0 and 1 (the
#'   Jacobian is infinite at 0 and 1). Percentages must be divided by 100.
#' * Variance parameters (especially spatial correlations) can change between
#'   transformations. Check the `converge` and `n_bound` columns, and the
#'   `varcomp` element, for the transformations you are choosing between.
#' * Heterogeneous variance between groups (e.g. environments) may be better
#'   modelled directly, with `dsum()` or `at()`, than transformed away.
#' * Back-transformed predictions from a transformed scale estimate medians
#'   rather than means.
#'
#' @returns An object of class `compare_transformations`, a list containing:
#' * `comparison`: data frame with one row per transformation, sorted by
#'   log-likelihood: `transformation`, `loglik`, `difference` (from the best),
#'   `converge`, `n_bound` (number of variance parameters at a boundary or
#'   singular), `note`, `lambda` (the power, for power transformations),
#'   `expression` (the transformed response, for refitting) and `mct_args`
#'   (the matching [multiple_comparisons()] arguments for back-transforming).
#' * `suggested`: the suggested transformation (see Details).
#' * `skipped`: named character vector of standard-set transformations that do
#'   not apply to the response, with the reason.
#' * `profile`: `NULL`, or for a Box-Cox profile a list with `profile` (data
#'   frame of `lambda`, `loglik`, `converge`, `n_bound`, `in_ci`),
#'   `lambda_hat`, `ci` (`NA` where the interval extends beyond the grid) and
#'   `suggested`.
#' * `varcomp`: long data frame of variance parameter estimates and boundary
#'   codes for each fit.
#' * `response`, `offset`, `conf_level`.
#'
#' @references
#' Box, G. E. P. and Cox, D. R. (1964). An analysis of transformations.
#' *Journal of the Royal Statistical Society: Series B*, 26(2), 211–252.
#'
#' Gurka, M. J., Edwards, L. J., Muller, K. E. and Kupper, L. L. (2006).
#' Extending the Box-Cox transformation to the linear mixed model. *Journal of
#' the Royal Statistical Society: Series A*, 169(2), 273–288.
#'
#' @seealso [autoplot.compare_transformations()], [multiple_comparisons()] for
#'   back-transforming predictions, [resplot()] for residual diagnostics.
#'
#' @importFrom stats qchisq update
#' @export
#'
#' @examplesIf requireNamespace("asreml", quietly = TRUE)
#' library(asreml)
#' dat <- asreml::oats
#' model <- asreml(
#'     yield ~ Nitrogen * Variety,
#'     random = ~ Blocks/Wplots,
#'     data = dat,
#'     trace = FALSE
#' )
#'
#' # All standard transformations
#' tr <- compare_transformations(model)
#' tr
#' autoplot(tr)
#'
#' # Add a cube root and a custom transformation to the standard set, plus a
#' # Box-Cox profile over a grid of powers
#' tr <- compare_transformations(
#'     model,
#'     trans = list("default", 1/3, log10p1 = function(y) log10(y + 1)),
#'     power = seq(-1, 2, by = 0.25)
#' )
#' tr
#' autoplot(tr)
compare_transformations <- function(
	model.obj,
	trans = "default",
	power = NULL,
	offset = NULL,
	conf_level = 0.95,
	max_refits = 5,
	env = parent.frame()
) {
	if (!inherits(model.obj, "asreml")) {
		stop("Only asreml models are supported.", call. = FALSE)
	}
	rlang::check_installed("asreml")

	specs <- trans_specs(trans)
	if (!is.null(power)) {
		if (!is.numeric(power) || anyNA(power) || length(unique(power)) < 3) {
			stop(
				"power must be a numeric vector of at least 3 distinct values ",
				"(a grid for the Box-Cox profile). For a single power, use a ",
				"number in trans, e.g. trans = list(\"log\", 1/3).",
				call. = FALSE
			)
		}
		power <- sort(unique(power))
	}

	cl <- match.call(asreml::asreml, model.obj$call)
	if (is.null(cl$data)) {
		stop("The model call must have a data argument.", call. = FALSE)
	}
	if (!is.null(cl$family)) {
		stop(
			"Transformation comparisons only apply to Gaussian models; ",
			"the model has a family argument.",
			call. = FALSE
		)
	}
	if (!is.null(cl$subset)) {
		stop(
			"Models fitted with a subset argument are not supported. ",
			"Subset the data before fitting instead.",
			call. = FALSE
		)
	}

	dat <- eval(cl$data, env)
	fixed <- eval(cl$fixed, env)
	resp <- trans_response(fixed)
	y <- dat[[resp]]
	if (!is.numeric(y)) {
		stop("Response '", resp, "' must be numeric.", call. = FALSE)
	}

	needs_positive <- !is.null(power) ||
		any(vapply(
			specs,
			function(s) identical(s$type, "power") && s$lambda != 1,
			logical(1)
		))
	if (is.null(offset)) {
		offset <- if (needs_positive) trans_auto_offset(y) else 0
	} else if (!is.numeric(offset) || length(offset) != 1 || !is.finite(offset)) {
		stop("offset must be a single finite number.", call. = FALSE)
	}

	cl[[1]] <- quote(asreml::asreml)
	cl$fixed <- stats::update(fixed, .ct_y ~ .)
	cl$data <- quote(.ct_dat)
	cl$trace <- FALSE

	# Do what the original fit did. If its call supplied G.param/R.param
	# (warm starts from update(), or deliberately fixed/constrained
	# parameters), refit with the model object's own copies, which keep the
	# constraint codes and don't depend on the call's expression being
	# findable. Otherwise fit from default starting values.
	start <- list()
	for (p in c("G.param", "R.param")) {
		if (!is.null(cl[[p]])) {
			nm <- paste0(".ct_", p)
			start[[nm]] <- model.obj[[p]]
			cl[[p]] <- as.name(nm)
		}
	}
	vc_bound <- as.character(summary(model.obj)$varcomp$bound)
	if (any(vc_bound == "F")) {
		warning(
			"The model has fixed variance parameters, which are held at the ",
			"same values for every transformation. Fixed variance ratios and ",
			"correlations are comparable across transformations; fixed ",
			"variances on the response scale are not.",
			call. = FALSE
		)
	}

	# Fits are cached so a power in both trans and power is fitted once
	cache <- list()
	fit_spec <- function(spec) {
		key <- if (spec$type == "power") {
			paste0("power:", spec$lambda)
		} else {
			paste0(spec$type, ":", spec$label)
		}
		if (is.null(cache[[key]])) {
			tv <- trans_values(spec, y, offset)
			if (is.null(tv$z)) {
				res <- trans_failed_fit(tv$note, inapplicable = TRUE)
			} else {
				d <- dat
				d$.ct_y <- tv$z
				res <- refit_transformed(cl, d, env, max_refits, start)
			}
			res$label <- spec$label
			cache[[key]] <<- res
		}
		return(cache[[key]])
	}

	fits <- lapply(specs, fit_spec)
	# Standard-set members that don't apply to the response are skipped
	# quietly; transformations the user named are always reported
	drop <- vapply(fits, `[[`, logical(1), "inapplicable") &
		vapply(specs, `[[`, logical(1), "from_default")
	skipped <- stats::setNames(
		vapply(fits[drop], `[[`, character(1), "note"),
		vapply(specs[drop], `[[`, character(1), "label")
	)
	specs <- specs[!drop]
	fits <- fits[!drop]
	info <- lapply(specs, trans_info, offset = offset, response = resp)
	comparison <- data.frame(
		transformation = vapply(specs, `[[`, character(1), "label"),
		loglik = vapply(fits, `[[`, numeric(1), "loglik"),
		difference = NA_real_,
		converge = vapply(fits, `[[`, logical(1), "converge"),
		n_bound = vapply(fits, `[[`, integer(1), "n_bound"),
		note = vapply(fits, `[[`, character(1), "note"),
		lambda = vapply(
			specs,
			function(s) if (s$type == "power") s$lambda else NA_real_,
			numeric(1)
		),
		expression = vapply(info, `[[`, character(1), "expression"),
		mct_args = vapply(info, `[[`, character(1), "mct_args"),
		stringsAsFactors = FALSE
	)
	ok <- comparison$converge & !is.na(comparison$loglik)
	h <- stats::qchisq(conf_level, 1) / 2
	suggested <- NA_character_
	if (any(ok)) {
		best <- max(comparison$loglik[ok])
		comparison$difference[ok] <- comparison$loglik[ok] - best
		within <- ok & comparison$loglik >= best - h
		suggested <- if (within[comparison$transformation == "none"]) {
			"none"
		} else {
			comparison$transformation[ok][which.max(comparison$loglik[ok])]
		}
	} else {
		warning("None of the transformations gave a converged fit.", call. = FALSE)
	}
	comparison <- comparison[order(-comparison$loglik, na.last = TRUE), ]
	rownames(comparison) <- NULL

	# Data-integrity check: the untransformed refit should reproduce the
	# original REML loglik when the original response was untransformed
	none <- comparison[comparison$transformation == "none", ]
	if (is.name(fixed[[2]]) && isTRUE(model.obj$converge) && isTRUE(none$converge)) {
		if (abs(none$loglik - model.obj$loglik) > 0.01) {
			warning(
				"The log-likelihood with no transformation (",
				round(none$loglik, 3),
				") differs from the original model's (",
				round(model.obj$loglik, 3),
				"). Check the data has not changed since fitting, or that the ",
				"original fit is not at a different local maximum.",
				call. = FALSE
			)
		}
	}

	profile <- NULL
	if (!is.null(power)) {
		pfits <- lapply(power, function(l) {
			fit_spec(list(
				label = paste0("power(", power_label(l), ")"),
				type = "power",
				lambda = l
			))
		})
		prof <- data.frame(
			lambda = power,
			loglik = vapply(pfits, `[[`, numeric(1), "loglik"),
			converge = vapply(pfits, `[[`, logical(1), "converge"),
			n_bound = vapply(pfits, `[[`, integer(1), "n_bound")
		)
		profile <- bc_summarise(prof, conf_level)
	}

	varcomp <- do.call(
		rbind,
		lapply(cache, function(f) {
			if (is.null(f$varcomp)) {
				return(NULL)
			}
			cbind(transformation = f$label, f$varcomp)
		})
	)
	rownames(varcomp) <- NULL

	out <- list(
		comparison = comparison,
		suggested = suggested,
		skipped = skipped,
		profile = profile,
		varcomp = varcomp,
		response = resp,
		offset = offset,
		conf_level = conf_level
	)
	class(out) <- "compare_transformations"
	return(out)
}

#' The standard set of transformations: those supported by
#' multiple_comparisons(). Expanded from the "default" keyword in `trans`.
#' @noRd
default_transformations <- c("none", "sqrt", "log", "inverse", "logit", "arcsin")

#' Parse the trans argument into transformation specifications
#'
#' Each spec is a list with `label`, `type` (`"power"`, `"logit"`, `"arcsin"`
#' or `"custom"`) and `from_default` (whether it came from the `"default"`
#' keyword), plus `lambda` for powers or `fun` for custom functions. A
#' transformation both in the standard set and named explicitly counts as
#' named. `"none"` is added as the baseline if missing.
#' @noRd
trans_specs <- function(trans) {
	if (is.character(trans) || is.numeric(trans)) {
		trans <- as.list(trans)
	}
	if (!is.list(trans) || length(trans) == 0) {
		stop(
			"trans must be a character vector or a list of transformations.",
			call. = FALSE
		)
	}
	nms <- names(trans)
	if (is.null(nms)) {
		nms <- rep("", length(trans))
	}

	specs <- list()
	for (i in seq_along(trans)) {
		t <- trans[[i]]
		if (identical(t, "default")) {
			for (d in default_transformations) {
				s <- parse_trans(d, "")
				s$from_default <- TRUE
				specs <- c(specs, list(s))
			}
		} else {
			s <- parse_trans(t, nms[i])
			s$from_default <- FALSE
			specs <- c(specs, list(s))
		}
	}

	labels <- vapply(specs, `[[`, character(1), "label")
	from_default <- vapply(specs, `[[`, logical(1), "from_default")
	keep <- !(from_default & labels %in% labels[!from_default])
	specs <- specs[keep]
	labels <- labels[keep]
	if (anyDuplicated(labels)) {
		stop(
			"Duplicate transformations in trans: ",
			paste(unique(labels[duplicated(labels)]), collapse = ", "),
			call. = FALSE
		)
	}
	if (!"none" %in% labels) {
		none <- list(label = "none", type = "power", lambda = 1, from_default = FALSE)
		specs <- c(list(none), specs)
	}
	return(specs)
}

#' Specification for one element of `trans`
#'
#' Numeric strings are read as powers, since `c("default", 1/3)` converts the
#' number to a string.
#' @noRd
parse_trans <- function(t, nm) {
	powers <- c(none = 1, sqrt = 0.5, log = 0, inverse = -1)
	if (is.character(t) && length(t) == 1) {
		num <- suppressWarnings(as.numeric(t))
		if (!is.na(num)) {
			t <- num
		}
	}
	if (is.function(t)) {
		if (!nzchar(nm)) {
			stop(
				"Custom transformations must be named, e.g. ",
				"trans = list(cuberoot = function(y) y^(1/3)).",
				call. = FALSE
			)
		}
		return(list(label = nm, type = "custom", fun = t))
	}
	if (is.numeric(t) && length(t) == 1 && is.finite(t)) {
		label <- if (nzchar(nm)) nm else paste0("power(", power_label(t), ")")
		return(list(label = label, type = "power", lambda = t))
	}
	if (is.character(t) && length(t) == 1) {
		if (t %in% names(powers)) {
			return(list(label = t, type = "power", lambda = powers[[t]]))
		}
		if (t %in% c("logit", "arcsin")) {
			return(list(label = t, type = t))
		}
		if (t == "power") {
			stop(
				"Give a power transformation as a number, e.g. ",
				"trans = c(\"default\", 1/3), or use the power argument for ",
				"a Box-Cox profile.",
				call. = FALSE
			)
		}
		stop(
			"Unknown transformation '",
			t,
			"'. Use 'default', one of 'none', 'sqrt', 'log', 'inverse', ",
			"'logit', 'arcsin', a number for a power, or a named function.",
			call. = FALSE
		)
	}
	stop(
		"Each element of trans must be a transformation name, a single ",
		"number (power) or a named function.",
		call. = FALSE
	)
}

#' Normalised transformed response z = g(y) / GM|g'(y)|
#'
#' The derivative of z has geometric mean 1, so REML (and ML) log-likelihoods
#' are comparable across transformations without a Jacobian term. Returns
#' `z = NULL` and a `note` when the data are outside the transformation's
#' domain.
#' @noRd
trans_values <- function(spec, y, offset) {
	ok <- !is.na(y)
	fail <- function(note) {
		return(list(z = NULL, note = note))
	}

	if (spec$type == "power") {
		lam <- spec$lambda
		if (lam == 1) {
			return(list(z = y, note = NA_character_))
		}
		u <- y + offset
		if (any(u[ok] <= 0)) {
			return(fail("needs positive values (after offset)"))
		}
		g <- if (abs(lam) < 1e-8) log(u) else (u^lam - 1) / lam
		dg <- u^(lam - 1)
	} else if (spec$type %in% c("logit", "arcsin")) {
		if (any(y[ok] <= 0 | y[ok] >= 1)) {
			return(fail("needs proportions in (0, 1)"))
		}
		if (spec$type == "logit") {
			g <- log(y / (1 - y))
			dg <- 1 / (y * (1 - y))
		} else {
			g <- asin(sqrt(y))
			dg <- 1 / (2 * sqrt(y * (1 - y)))
		}
	} else {
		# Out-of-domain values (e.g. NaN from log) are reported in the note
		g <- tryCatch(suppressWarnings(spec$fun(y)), error = function(e) NULL)
		if (!is.numeric(g) || length(g) != length(y) || any(!is.finite(g[ok]))) {
			return(fail("non-finite for the data"))
		}
		step <- 1e-6 * pmax(1, abs(y))
		dg <- suppressWarnings(
			(spec$fun(y + step) - spec$fun(y - step)) / (2 * step)
		)
		if (
			any(!is.finite(dg[ok])) ||
				any(dg[ok] == 0) ||
				length(unique(sign(dg[ok]))) > 1
		) {
			return(fail("not strictly monotone over the data"))
		}
	}

	z <- g / exp(mean(log(abs(dg[ok]))))
	return(list(z = z, note = NA_character_))
}

#' Raw response variable from the LHS of a fixed formula
#'
#' Resolves e.g. `log(yield)` to `yield`; errors where the LHS involves more
#' than one variable, since transforming any one of them would be wrong.
#' @noRd
trans_response <- function(fixed) {
	if (length(fixed) != 3) {
		stop("The fixed formula has no response.", call. = FALSE)
	}
	vars <- all.vars(fixed[[2]])
	if (length(vars) != 1) {
		stop(
			"The response must involve a single variable, but the LHS (",
			deparse(fixed[[2]]),
			") uses ",
			length(vars),
			". Create the response as a column in the data and refit.",
			call. = FALSE
		)
	}
	return(vars)
}

#' Automatic offset: 0 for positive data, otherwise a shift placing min(y) at
#' half the smallest positive gap above it
#' @noRd
trans_auto_offset <- function(y) {
	y <- y[!is.na(y)]
	mn <- min(y)
	if (mn > 0) {
		return(0)
	}
	d <- y - mn
	if (!any(d > 0)) {
		stop("The response is constant.", call. = FALSE)
	}
	return(-mn + min(d[d > 0]) / 2)
}

#' Result for a transformation that could not be fitted. `inapplicable` marks
#' one that does not apply to the data, as opposed to a fit that failed.
#' @noRd
trans_failed_fit <- function(note, inapplicable = FALSE) {
	return(list(
		loglik = NA_real_,
		converge = FALSE,
		n_bound = NA_integer_,
		note = note,
		inapplicable = inapplicable,
		varcomp = NULL
	))
}

#' Fit one transformed response, restarting from final values until converged
#' @noRd
refit_transformed <- function(cl, dat, env, max_refits, start = list()) {
	fit_env <- list2env(c(list(.ct_dat = dat), start), parent = env)
	m <- tryCatch(
		suppressWarnings(eval(cl, fit_env)),
		error = function(e) NULL
	)
	i <- 0
	while (!is.null(m) && !isTRUE(m$converge) && i < max_refits) {
		assign(".ct_prev", m, envir = fit_env)
		cl_re <- cl
		cl_re$G.param <- quote(.ct_prev$G.param)
		cl_re$R.param <- quote(.ct_prev$R.param)
		m <- tryCatch(
			suppressWarnings(eval(cl_re, fit_env)),
			error = function(e) NULL
		)
		i <- i + 1
	}
	if (is.null(m)) {
		return(trans_failed_fit("fit failed"))
	}

	vc <- summary(m)$varcomp
	varcomp <- data.frame(
		term = rownames(vc),
		component = vc$component,
		bound = as.character(vc$bound)
	)
	converge <- isTRUE(m$converge)
	return(list(
		loglik = m$loglik,
		converge = converge,
		n_bound = sum(varcomp$bound %in% c("B", "S")),
		note = if (converge) NA_character_ else "did not converge",
		inapplicable = FALSE,
		varcomp = varcomp
	))
}

#' Maximum, interpolated CI and suggested power from a Box-Cox profile
#'
#' Only converged fits are used. The interval is the contiguous region around
#' the maximum where the loglik is above the cut-off; a limit is NA if that
#' region reaches the end of the grid.
#' @noRd
bc_summarise <- function(prof, conf_level = 0.95) {
	ok <- prof$converge & !is.na(prof$loglik)
	prof$in_ci <- NA
	if (sum(ok) < 2) {
		warning(
			"Fewer than two Box-Cox powers gave converged fits.",
			call. = FALSE
		)
		return(list(
			profile = prof,
			lambda_hat = NA_real_,
			ci = c(lower = NA_real_, upper = NA_real_),
			suggested = NA_real_
		))
	}

	lam <- prof$lambda[ok]
	ll <- prof$loglik[ok]
	cut <- max(ll) - stats::qchisq(conf_level, 1) / 2
	prof$in_ci[ok] <- ll >= cut

	imax <- which.max(ll)
	lambda_hat <- lam[imax]

	crossing <- function(i, j) {
		return(lam[i] + (cut - ll[i]) * (lam[j] - lam[i]) / (ll[j] - ll[i]))
	}
	lo <- imax
	while (lo > 1 && ll[lo - 1] >= cut) {
		lo <- lo - 1
	}
	hi <- imax
	while (hi < length(ll) && ll[hi + 1] >= cut) {
		hi <- hi + 1
	}
	lower <- if (lo > 1) crossing(lo - 1, lo) else NA_real_
	upper <- if (hi < length(ll)) crossing(hi, hi + 1) else NA_real_

	if (imax == 1 || imax == length(ll)) {
		warning(
			"The Box-Cox maximum is at the edge of the power grid; ",
			"consider extending it.",
			call. = FALSE
		)
	}

	suggested <- bc_suggest(lambda_hat, lower, upper, range(lam))

	return(list(
		profile = prof,
		lambda_hat = lambda_hat,
		ci = c(lower = lower, upper = upper),
		suggested = suggested
	))
}

#' Interpretable power inside the interval: 1 if included, otherwise the
#' convenient value closest to the maximum. Open limits are treated as the
#' grid edge, so values outside the fitted grid are never suggested.
#' @noRd
bc_suggest <- function(lambda_hat, lower, upper, grid_range) {
	convenient <- c(-1, -0.5, 0, 1 / 3, 0.5, 1, 2)
	lo <- if (is.na(lower)) grid_range[1] else lower
	hi <- if (is.na(upper)) grid_range[2] else upper
	cand <- convenient[convenient >= lo & convenient <= hi]
	if (length(cand) == 0) {
		return(NA_real_)
	}
	if (1 %in% cand) {
		return(1)
	}
	return(cand[which.min(abs(cand - lambda_hat))])
}

#' Display label for a power
#' @noRd
power_label <- function(lambda) {
	if (isTRUE(all.equal(lambda, 1 / 3))) {
		return("1/3")
	}
	return(format(signif(lambda, 4)))
}

#' Descriptive name for a power
#' @noRd
power_name <- function(lambda) {
	names <- c(
		"-1" = "inverse",
		"-0.5" = "inverse square root",
		"0" = "log",
		"0.5" = "square root",
		"1" = "no transformation",
		"2" = "square"
	)
	if (isTRUE(all.equal(lambda, 1 / 3))) {
		return("cube root")
	}
	key <- as.character(lambda)
	return(if (key %in% names(names)) names[[key]] else "power")
}

#' Transformed-response expression and multiple_comparisons() arguments
#' @noRd
trans_info <- function(spec, offset, response) {
	off <- if (offset == 0) "" else paste0(" + ", signif(offset, 4))
	off_arg <- if (offset == 0) "" else paste0(", offset = ", signif(offset, 4))

	if (spec$type == "custom") {
		return(list(expression = NA_character_, mct_args = NA_character_))
	}
	if (spec$type == "logit") {
		return(list(
			expression = paste0("log(", response, " / (1 - ", response, "))"),
			mct_args = 'trans = "logit"'
		))
	}
	if (spec$type == "arcsin") {
		return(list(
			expression = paste0("asin(sqrt(", response, "))"),
			mct_args = 'trans = "arcsin"'
		))
	}

	lam <- spec$lambda
	if (lam == 1) {
		return(list(expression = response, mct_args = NA_character_))
	}
	u <- paste0(response, off)
	if (lam == 0) {
		return(list(
			expression = paste0("log(", u, ")"),
			mct_args = paste0('trans = "log"', off_arg)
		))
	}
	if (lam == 0.5) {
		return(list(
			expression = paste0("sqrt(", u, ")"),
			mct_args = paste0('trans = "sqrt"', off_arg)
		))
	}
	# back_transform() ignores the offset for "inverse", so use "power" then
	if (lam == -1 && offset == 0) {
		return(list(
			expression = paste0("1 / ", response),
			mct_args = 'trans = "inverse"'
		))
	}
	pw <- power_label(lam)
	base <- if (offset == 0) response else paste0("(", u, ")")
	exponent <- if (lam < 0 || grepl("/", pw)) paste0("(", pw, ")") else pw
	return(list(
		expression = paste0(base, "^", exponent),
		mct_args = paste0('trans = "power", power = ', pw, off_arg)
	))
}

#' Print how to use a suggested transformation
#' @noRd
cat_refit_hint <- function(expression, mct_args) {
	if (is.na(expression)) {
		cat("  Back-transform custom transformations manually.\n")
		return(invisible(NULL))
	}
	cat("  Refit with the response ", expression, "\n", sep = "")
	if (!is.na(mct_args)) {
		cat(
			"  and back-transform with multiple_comparisons(..., ",
			mct_args,
			")\n",
			sep = ""
		)
	}
	return(invisible(NULL))
}

#' @rdname compare_transformations
#' @param x A `compare_transformations` object.
#' @param ... Not used.
#' @export
print.compare_transformations <- function(x, ...) {
	fmt <- function(v) {
		return(ifelse(is.na(v), "NA", formatC(v, format = "f", digits = 2)))
	}
	h <- stats::qchisq(x$conf_level, 1) / 2
	tab <- x$comparison

	cat("Response transformations for '", x$response, "'\n", sep = "")
	cat("REML log-likelihoods, normalised to be comparable\n")
	if (x$offset != 0) {
		cat(
			"Offset added before power transformations:",
			signif(x$offset, 4),
			"\n"
		)
	}
	cat("\n")

	note <- ifelse(
		!is.na(tab$note),
		tab$note,
		ifelse(
			!is.na(tab$n_bound) & tab$n_bound > 0,
			paste(tab$n_bound, "parameter(s) at a boundary"),
			""
		)
	)
	# Pad by hand: text left-aligned, numbers right-aligned
	disp <- data.frame(
		Transformation = format(tab$transformation),
		logLik = formatC(fmt(tab$loglik), width = 8),
		Difference = formatC(fmt(tab$difference), width = 10),
		Note = note,
		check.names = FALSE
	)
	print(disp, row.names = FALSE, right = FALSE)

	cat(
		"\nTransformations within",
		formatC(h, format = "f", digits = 2),
		"of the best are not clearly distinguishable.\n"
	)
	if (is.na(x$suggested)) {
		cat("No transformation gave a converged fit.\n")
	} else if (x$suggested == "none") {
		cat("Suggested: no transformation\n")
	} else {
		row <- tab[tab$transformation == x$suggested, ]
		cat("Suggested: ", x$suggested, "\n", sep = "")
		cat_refit_hint(row$expression, row$mct_args)
	}
	if (length(x$skipped) > 0) {
		cat("\nNot applicable to this response:\n")
		for (reason in unique(x$skipped)) {
			cat(
				"  ",
				paste(names(x$skipped)[x$skipped == reason], collapse = ", "),
				" (",
				reason,
				")\n",
				sep = ""
			)
		}
	}

	if (!is.null(x$profile)) {
		p <- x$profile
		prof <- p$profile
		cat("\nBox-Cox profile over", nrow(prof), "powers")
		cat(" (", sum(prof$converge), " converged)\n", sep = "")
		lower <- if (is.na(p$ci[1])) {
			paste0("< ", fmt(min(prof$lambda)))
		} else {
			fmt(p$ci[1])
		}
		upper <- if (is.na(p$ci[2])) {
			paste0("> ", fmt(max(prof$lambda)))
		} else {
			fmt(p$ci[2])
		}
		cat(
			"  Maximum at power = ",
			fmt(p$lambda_hat),
			"; approximate ",
			100 * x$conf_level,
			"% CI [",
			lower,
			", ",
			upper,
			"]\n",
			sep = ""
		)
		if (is.na(p$suggested)) {
			cat("  No conventional power lies within the interval.\n")
		} else {
			cat(
				"  Suggested power: ",
				power_label(p$suggested),
				" (",
				power_name(p$suggested),
				")\n",
				sep = ""
			)
			if (p$suggested != 1) {
				info <- trans_info(
					list(type = "power", lambda = p$suggested),
					x$offset,
					x$response
				)
				cat_refit_hint(info$expression, info$mct_args)
			}
		}
		if (any(!prof$converge)) {
			cat(
				"  Not converged at power =",
				paste(prof$lambda[!prof$converge], collapse = ", "),
				"\n"
			)
		}
		b <- prof$lambda[!is.na(prof$n_bound) & prof$n_bound > 0]
		if (length(b) > 0) {
			cat(
				"  Variance parameters at a boundary at power =",
				paste(b, collapse = ", "),
				"\n"
			)
		}
	}
	cat(
		"\nThe log-likelihood compares transformations; it does not show that\n",
		"any is adequate. Check the residuals of the refitted model with ",
		"resplot().\n",
		sep = ""
	)
	return(invisible(x))
}

#' @rdname autoplot
#' @param object A `compare_transformations` object, as produced by
#'   [compare_transformations()]. With a Box-Cox profile, plots the profile
#'   log-likelihood against the power with its confidence interval shaded,
#'   named power transformations marked, and other transformations as
#'   horizontal reference lines. Otherwise, plots the log-likelihood of each
#'   transformation, shading those not clearly distinguishable from the best.
#' @returns A `ggplot2` object.
#' @importFrom ggplot2 autoplot ggplot aes geom_line geom_point geom_text geom_hline geom_vline annotate labs theme_bw scale_shape_manual
#' @importFrom rlang .data
#' @export
autoplot.compare_transformations <- function(object, ...) {
	h <- stats::qchisq(object$conf_level, 1) / 2
	tab <- object$comparison
	tab <- tab[tab$converge & !is.na(tab$loglik), ]
	ylab <- "REML log-likelihood (normalised)"

	if (is.null(object$profile)) {
		tab$transformation <- factor(
			tab$transformation,
			levels = rev(tab$transformation)
		)
		best <- max(tab$loglik)
		p <- ggplot2::ggplot(
			tab,
			ggplot2::aes(x = .data$loglik, y = .data$transformation)
		) +
			ggplot2::annotate(
				"rect",
				xmin = best - h,
				xmax = best,
				ymin = -Inf,
				ymax = Inf,
				alpha = 0.15,
				fill = "steelblue"
			) +
			ggplot2::geom_point(size = 2.5) +
			ggplot2::labs(x = ylab, y = NULL) +
			ggplot2::theme_bw()
		return(p)
	}

	prof <- object$profile$profile
	prof$Converged <- ifelse(prof$converge, "Yes", "No")
	okp <- prof$converge & !is.na(prof$loglik)
	cut <- max(prof$loglik[okp]) - h
	rng <- range(prof$lambda)
	ci <- object$profile$ci
	lo <- if (is.na(ci[1])) rng[1] else ci[1]
	hi <- if (is.na(ci[2])) rng[2] else ci[2]

	named_pw <- tab[!is.na(tab$lambda) & tab$lambda >= rng[1] & tab$lambda <= rng[2], ]
	other <- tab[is.na(tab$lambda), ]

	p <- ggplot2::ggplot(
		prof[!is.na(prof$loglik), ],
		ggplot2::aes(x = .data$lambda, y = .data$loglik)
	) +
		ggplot2::annotate(
			"rect",
			xmin = lo,
			xmax = hi,
			ymin = -Inf,
			ymax = Inf,
			alpha = 0.15,
			fill = "steelblue"
		) +
		ggplot2::geom_hline(yintercept = cut, linetype = "dashed") +
		ggplot2::geom_line() +
		ggplot2::geom_point(ggplot2::aes(shape = .data$Converged), size = 2) +
		ggplot2::scale_shape_manual(values = c(Yes = 16, No = 1)) +
		ggplot2::labs(
			x = expression("Box-Cox power" ~ (lambda)),
			y = ylab,
			shape = "Converged"
		) +
		ggplot2::theme_bw()

	if (nrow(named_pw) > 0) {
		p <- p +
			ggplot2::geom_point(
				data = named_pw,
				colour = "firebrick",
				size = 3,
				shape = 4,
				stroke = 1
			) +
			ggplot2::geom_text(
				data = named_pw,
				ggplot2::aes(label = .data$transformation),
				colour = "firebrick",
				vjust = -1,
				size = 3.5
			)
	}
	if (nrow(other) > 0) {
		p <- p +
			ggplot2::geom_hline(
				data = other,
				ggplot2::aes(yintercept = .data$loglik),
				linetype = "dotdash",
				colour = "grey40"
			) +
			ggplot2::geom_text(
				data = other,
				ggplot2::aes(x = rng[1], label = .data$transformation),
				hjust = 0,
				vjust = -0.5,
				colour = "grey30",
				size = 3.5
			)
	}
	return(p)
}
