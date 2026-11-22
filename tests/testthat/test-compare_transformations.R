# Helpers (no asreml needed) ----------------------------------------------

test_that("trans_values matches the normalised Box-Cox transform", {
	y <- c(1.5, 2, 3.2, NA, 8)
	gm <- exp(mean(log(y), na.rm = TRUE))
	pw <- function(l) list(label = "p", type = "power", lambda = l)
	expect_equal(trans_values(pw(0), y, 0)$z, gm * log(y))
	expect_equal(
		trans_values(pw(0.5), y, 0)$z,
		(y^0.5 - 1) / (0.5 * gm^(0.5 - 1))
	)
	expect_equal(trans_values(pw(1), y, 0)$z, y)
	expect_equal(
		trans_values(pw(1e-6), y, 0)$z,
		trans_values(pw(0), y, 0)$z,
		tolerance = 1e-4
	)
})

test_that("normalised transforms give comparable ML log-likelihoods", {
	set.seed(42)
	x <- rep(1:5, each = 4)
	y <- exp(0.3 * x + rnorm(20, sd = 0.3))
	pw <- function(l) list(label = "p", type = "power", lambda = l)
	for (l in c(-1, 0, 0.5, 2)) {
		ll_norm <- as.numeric(logLik(lm(trans_values(pw(l), y, 0)$z ~ x)))
		raw <- if (l == 0) log(y) else (y^l - 1) / l
		ll_adj <- as.numeric(logLik(lm(raw ~ x))) + (l - 1) * sum(log(y))
		expect_equal(ll_norm, ll_adj, tolerance = 1e-8)
	}
})

test_that("normalisation makes REML log-likelihoods invariant to rescaling", {
	skip_if_not_installed("nlme")
	set.seed(1)
	d <- data.frame(g = gl(6, 5), x = rnorm(30))
	d$y <- exp(1 + 0.3 * as.integer(d$g) + 0.2 * d$x + rnorm(30, sd = 0.3))
	reml <- function(z) {
		d$z <- z
		return(as.numeric(logLik(nlme::gls(z ~ g + x, data = d, method = "REML"))))
	}
	log_spec <- list(label = "log", type = "power", lambda = 0)
	custom <- list(
		label = "scaled",
		type = "custom",
		fun = function(y) 10 * log(y) + 3
	)
	expect_equal(
		reml(trans_values(custom, d$y, 0)$z),
		reml(trans_values(log_spec, d$y, 0)$z),
		tolerance = 1e-6
	)
})

test_that("trans_values checks the domain of each transformation", {
	pw <- list(label = "log", type = "power", lambda = 0)
	expect_match(trans_values(pw, c(0, 1, 2), 0)$note, "positive")
	expect_false(is.null(trans_values(pw, c(0, 1, 2), 0.5)$z))
	for (type in c("logit", "arcsin")) {
		sp <- list(label = type, type = type)
		expect_match(trans_values(sp, c(0, 0.5, 0.9), 0)$note, "proportions")
		expect_match(trans_values(sp, c(10, 50, 90), 0)$note, "proportions")
		expect_false(is.null(trans_values(sp, c(0.1, 0.5, 0.9), 0)$z))
	}
	nonmono <- list(label = "sq", type = "custom", fun = function(y) y^2)
	expect_match(
		trans_values(nonmono, c(-1, 0.5, 2), 0)$note,
		"not strictly monotone"
	)
	bad <- list(label = "b", type = "custom", fun = function(y) log(y))
	expect_match(trans_values(bad, c(-1, 1, 2), 0)$note, "non-finite")
	expect_no_warning(trans_values(bad, c(-1, 1, 2), 0))
})

test_that("logit and arcsin use the correct derivatives", {
	y <- c(0.1, 0.25, 0.6, 0.9)
	num_d <- function(f) (f(y + 1e-6) - f(y - 1e-6)) / 2e-6
	for (type in c("logit", "arcsin")) {
		f <- if (type == "logit") qlogis else function(p) asin(sqrt(p))
		analytic <- trans_values(list(label = type, type = type), y, 0)$z
		custom <- trans_values(list(label = type, type = "custom", fun = f), y, 0)$z
		expect_equal(analytic, custom, tolerance = 1e-6)
	}
})

test_that("trans_specs parses names, powers and functions", {
	s <- trans_specs(list("log", 1 / 3, cr = function(y) y^(1 / 3), "logit"))
	labels <- vapply(s, `[[`, character(1), "label")
	expect_equal(labels, c("none", "log", "power(1/3)", "cr", "logit"))
	expect_equal(s[[2]]$lambda, 0)
	expect_equal(s[[3]]$type, "power")
	expect_equal(s[[4]]$type, "custom")
	expect_equal(trans_specs(c("none", "sqrt"))[[1]]$label, "none")

	# The "default" keyword expands to the standard set, marked as such
	d <- trans_specs("default")
	expect_equal(
		vapply(d, `[[`, character(1), "label"),
		c("none", "sqrt", "log", "inverse", "logit", "arcsin")
	)
	expect_true(all(vapply(d, `[[`, logical(1), "from_default")))
	# Naming a standard-set member explicitly takes precedence
	d2 <- trans_specs(c("default", "logit"))
	labels2 <- vapply(d2, `[[`, character(1), "label")
	expect_equal(sum(labels2 == "logit"), 1)
	expect_false(d2[[which(labels2 == "logit")]]$from_default)
	# c() turns numbers into strings; they are still read as powers
	d3 <- trans_specs(c("default", 1 / 3))
	expect_equal(d3[[7]]$label, "power(1/3)")
	expect_equal(d3[[7]]$lambda, 1 / 3)

	expect_error(trans_specs("boxcox"), "Unknown transformation")
	expect_error(trans_specs(c("default", "default")), "Duplicate")
	expect_error(trans_specs("power"), "as a number")
	expect_error(trans_specs(list(function(y) y)), "must be named")
	expect_error(trans_specs(c("log", "log")), "Duplicate")
	expect_error(trans_specs(list(c(1, 2))), "Each element")
})

test_that("trans_auto_offset handles positive, zero and negative responses", {
	expect_equal(trans_auto_offset(c(1, 2, 3)), 0)
	expect_equal(trans_auto_offset(c(0, 0, 0.2, 1, NA)), 0.1)
	expect_equal(trans_auto_offset(c(-2, -1, 3)), 2.5)
	expect_error(trans_auto_offset(c(0, 0, NA)), "constant")
})

test_that("trans_response extracts the raw variable from the LHS", {
	expect_equal(trans_response(yield ~ Variety), "yield")
	expect_equal(trans_response(log(yield + 1) ~ Variety), "yield")
	expect_error(trans_response(~Variety), "no response")
	expect_error(trans_response(I(yield / area) ~ Variety), "single variable")
})

test_that("bc_summarise interpolates the interval and suggests a power", {
	lam <- seq(-1, 2, by = 0.25)
	prof <- data.frame(
		lambda = lam,
		loglik = -4 * abs(lam - 0.5),
		converge = TRUE,
		n_bound = 0L
	)
	s <- bc_summarise(prof)
	h <- qchisq(0.95, 1) / 2
	expect_equal(s$lambda_hat, 0.5)
	expect_equal(unname(s$ci), c(0.5 - h / 4, 0.5 + h / 4))
	expect_equal(s$suggested, 0.5)
	expect_equal(s$profile$in_ci, abs(lam - 0.5) * 4 <= h)
})

test_that("bc_summarise ignores non-converged fits", {
	lam <- seq(-1, 2, by = 0.25)
	prof <- data.frame(
		lambda = lam,
		loglik = -4 * abs(lam - 0.5),
		converge = TRUE,
		n_bound = 0L
	)
	prof$loglik[lam == 1.5] <- 10 # spurious, unconverged
	prof$converge[lam == 1.5] <- FALSE
	prof$loglik[lam == 2] <- NA
	prof$converge[lam == 2] <- FALSE
	s <- bc_summarise(prof)
	expect_equal(s$lambda_hat, 0.5)
	expect_true(is.na(s$profile$in_ci[lam == 1.5]))
})

test_that("bc_summarise flags open intervals and edge maxima", {
	lam <- seq(-1, 1, by = 0.25)
	prof <- data.frame(
		lambda = lam,
		loglik = -0.5 * abs(lam - 1),
		converge = TRUE,
		n_bound = 0L
	)
	expect_warning(s <- bc_summarise(prof), "edge of the power grid")
	expect_true(is.na(s$ci[["upper"]]))
	expect_equal(s$suggested, 1)
})

test_that("bc_summarise warns when too few fits converged", {
	prof <- data.frame(
		lambda = c(0, 0.5, 1),
		loglik = c(-1, NA, NA),
		converge = c(TRUE, FALSE, FALSE),
		n_bound = 0L
	)
	expect_warning(s <- bc_summarise(prof), "Fewer than two")
	expect_true(is.na(s$lambda_hat))
})

test_that("bc_suggest prefers 1, then the nearest convenient power", {
	expect_equal(bc_suggest(0.8, 0.4, 1.3, c(-1, 2)), 1)
	expect_equal(bc_suggest(0.1, -0.3, 0.45, c(-1, 2)), 0)
	expect_equal(bc_suggest(0.6, 0.55, 0.65, c(-1, 2)), NA_real_)
	# open lower limit is clipped to the grid
	expect_equal(bc_suggest(-1, NA, -0.7, c(-1, 2)), -1)
})

test_that("trans_info gives the refit expression and multiple_comparisons() arguments", {
	pw <- function(l) list(type = "power", lambda = l)
	expect_equal(trans_info(pw(0), 0, "y")$mct_args, 'trans = "log"')
	expect_equal(trans_info(pw(0), 0.5, "y")$expression, "log(y + 0.5)")
	expect_equal(
		trans_info(pw(0.5), 0.5, "y")$mct_args,
		'trans = "sqrt", offset = 0.5'
	)
	expect_equal(trans_info(pw(-1), 0, "y")$mct_args, 'trans = "inverse"')
	expect_equal(
		trans_info(pw(-1), 0.5, "y")$mct_args,
		'trans = "power", power = -1, offset = 0.5'
	)
	expect_equal(trans_info(pw(1 / 3), 0, "y")$expression, "y^(1/3)")
	expect_equal(trans_info(pw(1), 0, "y")$expression, "y")
	expect_equal(
		trans_info(list(type = "logit"), 0, "y")$mct_args,
		'trans = "logit"'
	)
	expect_true(is.na(trans_info(list(type = "custom"), 0, "y")$expression))
})

test_that("compare_transformations rejects non-asreml models", {
	m <- lm(Petal.Length ~ Species, data = iris)
	expect_error(
		compare_transformations(m),
		"Only asreml models are supported"
	)
})

# asreml ------------------------------------------------------------------

load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)

test_that("compare_transformations refits asreml models correctly", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	tr <- compare_transformations(
		model.asr,
		trans = list("sqrt", "log", 1 / 3, l10 = function(y) log10(y))
	)
	expect_s3_class(tr, "compare_transformations")
	expect_equal(tr$response, "yield")
	expect_equal(tr$offset, 0)
	expect_setequal(
		tr$comparison$transformation,
		c("none", "sqrt", "log", "power(1/3)", "l10")
	)
	expect_true(all(tr$comparison$converge))
	# No transformation reproduces the original REML log-likelihood
	ll <- setNames(tr$comparison$loglik, tr$comparison$transformation)
	expect_equal(ll[["none"]], model.asr$loglik, tolerance = 1e-3)
	# log10 is a rescaled log, so normalisation makes them identical
	expect_equal(ll[["l10"]], ll[["log"]], tolerance = 1e-3)
	expect_equal(max(tr$comparison$difference), 0)
	expect_null(tr$profile)
	expect_output(print(tr), "Response transformations for 'yield'")
	expect_s3_class(autoplot(tr), "ggplot")
})

test_that("compare_transformations profiles Box-Cox powers and shares fits", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	tr <- compare_transformations(
		model.asr,
		trans = c("none", "log"),
		power = c(0, 0.5, 1, 1.5)
	)
	p <- tr$profile$profile
	ll <- setNames(tr$comparison$loglik, tr$comparison$transformation)
	expect_equal(p$loglik[p$lambda == 0], ll[["log"]])
	expect_equal(p$loglik[p$lambda == 1], ll[["none"]])
	expect_true(!is.na(tr$profile$lambda_hat))
	expect_output(print(tr), "Box-Cox profile over 4 powers")
	expect_s3_class(autoplot(tr), "ggplot")
})

test_that("the default set skips transformations that don't apply", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	m <- asreml(
		yield ~ Nitrogen + Variety,
		random = ~Blocks,
		data = dat,
		trace = FALSE
	)
	tr <- compare_transformations(m)
	expect_setequal(
		tr$comparison$transformation,
		c("none", "sqrt", "log", "inverse")
	)
	expect_setequal(names(tr$skipped), c("logit", "arcsin"))
	expect_match(tr$skipped[["logit"]], "proportions")
	expect_output(print(tr), "Not applicable to this response")
	expect_output(print(tr), "resplot()", fixed = TRUE)

	# Adding to the standard set keeps the quiet skipping; naming logit
	# explicitly reports it with a note instead
	tr2 <- compare_transformations(m, trans = c("default", 1 / 3, "logit"))
	expect_true("power(1/3)" %in% tr2$comparison$transformation)
	expect_equal(names(tr2$skipped), "arcsin")
	logit <- tr2$comparison[tr2$comparison$transformation == "logit", ]
	expect_match(logit$note, "proportions")

	# A proportion response gets every default transformation
	pdat <- dat
	pdat$prop <- (dat$yield - min(dat$yield) + 1) /
		(max(dat$yield) - min(dat$yield) + 2)
	mp <- asreml(
		prop ~ Nitrogen + Variety,
		random = ~Blocks,
		data = pdat,
		trace = FALSE
	)
	trp <- compare_transformations(mp)
	expect_length(trp$skipped, 0)
	expect_setequal(
		trp$comparison$transformation,
		c("none", "sqrt", "log", "inverse", "logit", "arcsin")
	)
})

test_that("compare_transformations notes transformations outside their domain", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	tr <- compare_transformations(model.asr, trans = c("log", "logit"))
	logit <- tr$comparison[tr$comparison$transformation == "logit", ]
	expect_true(is.na(logit$loglik))
	expect_match(logit$note, "proportions")
	expect_false(is.na(tr$suggested))
})

test_that("compare_transformations resolves the raw response from a transformed LHS", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	m_log <- asreml(
		log(yield) ~ Nitrogen + Variety,
		random = ~Blocks,
		data = dat,
		trace = FALSE
	)
	m_raw <- asreml(
		yield ~ Nitrogen + Variety,
		random = ~Blocks,
		data = dat,
		trace = FALSE
	)
	tr_log <- compare_transformations(m_log)
	tr_raw <- compare_transformations(m_raw)
	expect_equal(tr_log$response, "yield")
	expect_equal(tr_log$comparison, tr_raw$comparison, tolerance = 1e-6)
})

test_that("compare_transformations finds data local to the calling function", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	f <- function() {
		local_dat <- dat
		local_dat$yield <- local_dat$yield - 60 # some non-positive values
		m <- asreml(
			yield ~ Nitrogen + Variety,
			random = ~Blocks,
			data = local_dat,
			trace = FALSE
		)
		return(compare_transformations(m))
	}
	tr <- f()
	expect_equal(tr$offset, trans_auto_offset(dat$yield - 60))
	expect_true(tr$offset > 0)
	expect_false(anyNA(tr$comparison$loglik))
})

test_that("compare_transformations keeps deliberately fixed parameters from G.param", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	sv <- asreml(
		yield ~ Nitrogen + Variety,
		random = ~Blocks,
		data = dat,
		start.values = TRUE,
		trace = FALSE
	)
	gt <- sv$vparameters.table
	gt$Constraint[gt$Component == "Blocks"] <- "F"
	gt$Value[gt$Component == "Blocks"] <- 2
	m_fixed <- asreml(
		yield ~ Nitrogen + Variety,
		random = ~Blocks,
		G.param = gt,
		data = dat,
		trace = FALSE
	)
	expect_warning(
		tr <- compare_transformations(m_fixed),
		"fixed variance parameters"
	)
	# Refits use the same constrained model, so no transformation reproduces it
	none <- tr$comparison[tr$comparison$transformation == "none", ]
	expect_equal(none$loglik, m_fixed$loglik, tolerance = 1e-3)
	blocks <- tr$varcomp[tr$varcomp$term == "Blocks", ]
	expect_true(all(blocks$bound == "F"))
})

test_that("compare_transformations validates its arguments", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	# Power transformations of non-positive values are noted, not errors
	tr <- compare_transformations(model.asr, trans = c("none", "log"), offset = -1000)
	logt <- tr$comparison[tr$comparison$transformation == "log", ]
	expect_match(logt$note, "positive")
	expect_equal(tr$suggested, "none")
	expect_error(compare_transformations(model.asr, power = 1), "at least 3")
	expect_error(
		compare_transformations(model.asr, offset = c(1, 2)),
		"single finite number"
	)
})

test_that("refit_transformed returns a failed fit rather than an error", {
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	cl <- quote(asreml::asreml(
		fixed = .ct_y ~ Variety,
		random = ~not_a_column,
		data = .ct_dat,
		trace = FALSE
	))
	d <- dat
	d$.ct_y <- d$yield
	res <- refit_transformed(cl, d, globalenv(), max_refits = 1)
	expect_true(is.na(res$loglik))
	expect_false(res$converge)
	expect_equal(res$note, "fit failed")
})
