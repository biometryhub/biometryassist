test_that("reference_comparisons returns a means-centric table with expected columns", {
	m <- aov(weight ~ feed, data = chickwts)
	out <- reference_comparisons(m, classify = "feed", reference = "casein")

	expect_s3_class(out, "reference_comparisons")
	expect_s3_class(out, "data.frame")
	expect_true(all(
		c(
			"level1",
			"level2",
			"comparison",
			"estimate",
			"level1.mean",
			"level2.mean",
			"std.error",
			"statistic",
			"df",
			"p.value",
			"conf.low",
			"conf.high"
		) %in%
			names(out)
	))
	# 6 feeds => 5 comparisons against the reference (no self-row)
	expect_equal(nrow(out), 5)
	# the reference is always level2, and is not compared to itself
	expect_true(all(out$level2 == "casein"))
	expect_false("casein" %in% out$level1)
	# the means decompose the estimate, and level2.mean is the (constant) ref mean
	expect_equal(out$estimate, out$level1.mean - out$level2.mean)
	expect_equal(length(unique(out$level2.mean)), 1L)
	expect_equal(attr(out, "reference"), "casein")
	expect_equal(attr(out, "comparison_method"), "dunnett")
})

test_that("Dunnett p-values match an independent emmeans trt.vs.ctrl (mvt)", {
	skip_if_not_installed("emmeans")
	m <- aov(weight ~ feed, data = chickwts)

	set.seed(123)
	out <- reference_comparisons(m, classify = "feed", reference = "casein")

	emm <- emmeans::emmeans(m, ~feed)
	set.seed(123)
	ref <- as.data.frame(emmeans::contrast(
		emm,
		method = "trt.vs.ctrl",
		ref = 1,
		adjust = "mvt"
	))

	ours <- stats::setNames(out$p.value, gsub(" ", "", out$comparison))
	theirs <- stats::setNames(ref$p.value, gsub(" - ", "-", ref$contrast))
	expect_equal(ours[names(theirs)], theirs, tolerance = 5e-3)

	# estimates and SEs are exact (no Monte Carlo)
	our_est <- stats::setNames(out$estimate, gsub(" ", "", out$comparison))
	their_est <- stats::setNames(ref$estimate, gsub(" - ", "-", ref$contrast))
	expect_equal(our_est[names(their_est)], their_est, tolerance = 1e-6)
})

test_that("dunnett simultaneous CIs agree with significance", {
	m <- aov(weight ~ feed, data = chickwts)
	out <- reference_comparisons(m, classify = "feed", reference = "casein")
	excludes_zero <- out$conf.low > 0 | out$conf.high < 0
	expect_equal(excludes_zero, out$p.value < attr(out, "sig_level"))
})

test_that("a p.adjust method is honoured (with a message) and equals pairwise all-vs-ref", {
	m <- aov(weight ~ feed, data = chickwts)

	expect_message(
		rc <- reference_comparisons(
			m,
			classify = "feed",
			reference = "casein",
			adjust = "holm"
		),
		"dunnett"
	)
	expect_equal(attr(rc, "comparison_method"), "holm")

	others <- setdiff(levels(chickwts$feed), "casein")
	pc <- pairwise_comparisons(
		m,
		classify = "feed",
		pairs = lapply(others, function(o) c(o, "casein")),
		adjust = "holm"
	)
	# align on comparison label
	rc_p <- stats::setNames(rc$p.value, rc$comparison)
	pc_p <- stats::setNames(pc$p.value, pc$comparison)
	expect_equal(rc_p[names(pc_p)], pc_p, tolerance = 1e-8)
	rc_e <- stats::setNames(rc$estimate, rc$comparison)
	pc_e <- stats::setNames(pc$estimate, pc$comparison)
	expect_equal(rc_e[names(pc_e)], pc_e, tolerance = 1e-8)
})

test_that("adjust = 'tukey' is rejected", {
	m <- aov(weight ~ feed, data = chickwts)
	expect_error(
		reference_comparisons(
			m,
			classify = "feed",
			reference = "casein",
			adjust = "tukey"
		),
		"multiple_comparisons"
	)
})

test_that("a misspelled argument is caught (check_dots_used), not silently ignored", {
	m <- aov(weight ~ feed, data = chickwts)
	expect_error(
		reference_comparisons(
			m,
			classify = "feed",
			reference = "casein",
			adjsut = "holm"
		),
		"must be used"
	)
})

test_that("include_means = FALSE is ignored with a warning", {
	m <- aov(weight ~ feed, data = chickwts)
	expect_warning(
		out <- reference_comparisons(
			m,
			classify = "feed",
			reference = "casein",
			include_means = FALSE
		),
		"include_means"
	)
	expect_true(all(c("level1.mean", "level2.mean") %in% names(out)))
})

test_that("aliased levels are stored and an aliased reference errors clearly", {
	# Empty A:Y cell -> A:Y not estimable -> aliased and dropped
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B")),
		Site = factor(c("X", "Y")),
		rep = 1:5
	)
	d <- d[!(d$Trt == "A" & d$Site == "Y"), ]
	d$y <- rnorm(nrow(d), as.numeric(d$Trt) + as.numeric(d$Site))
	m <- aov(y ~ Trt * Site, data = d)

	out <- suppressWarnings(reference_comparisons(
		m,
		classify = "Trt:Site",
		reference = "B:X"
	))
	expect_identical(attr(out, "aliased"), "A:Y")
	# reported once on print (shared note, same wording as multiple_comparisons)
	expect_output(print(out), "Aliased level is: A:Y")

	# using the aliased level as the reference gives a clear error
	err <- tryCatch(
		suppressWarnings(reference_comparisons(
			m,
			classify = "Trt:Site",
			reference = "A:Y"
		)),
		error = function(e) conditionMessage(e)
	)
	expect_match(err, "[Aa]liased")
})

test_that("an unknown reference level errors", {
	m <- aov(weight ~ feed, data = chickwts)
	expect_error(
		reference_comparisons(m, classify = "feed", reference = "nope"),
		"not found"
	)
})

test_that("by runs within groups; a group missing the reference warns and skips", {
	set.seed(1)
	d <- expand.grid(
		trt = factor(c("ctrl", "a", "b")),
		site = factor(c("S1", "S2")),
		rep = 1:5
	)
	d$y <- rnorm(nrow(d)) + as.numeric(d$trt)
	m <- aov(y ~ trt * site, data = d)

	out <- reference_comparisons(
		m,
		classify = "trt:site",
		reference = "ctrl",
		by = "site"
	)
	expect_true("site" %in% names(out))
	# 2 non-ref trts x 2 sites
	expect_equal(nrow(out), 4)
	expect_setequal(as.character(unique(out$site)), c("S1", "S2"))
	expect_true(all(out$level2 == "ctrl"))
})

test_that("comparison-specific df (aovlist) falls back to holm with a warning", {
	# An Error() stratum yields comparison-specific df, so exact Dunnett is not
	# available -> fall back to Holm. Uses a built-in dataset so emmeans can
	# re-fit the aovlist model.
	m <- aov(yield ~ N + Error(block), data = npk)

	expect_warning(
		out <- reference_comparisons(m, classify = "N", reference = "0"),
		"Falling back"
	)
	expect_equal(attr(out, "comparison_method"), "holm")
})

test_that("transformed response warns it is reported on the model scale", {
	# reference_comparisons() has no `trans` argument, so the warning must not
	# tell the user to specify one; it reports differences on the model scale.
	m <- aov(log(weight) ~ feed, data = chickwts)
	expect_warning(
		reference_comparisons(m, classify = "feed", reference = "casein"),
		"model \\(transformed\\) scale"
	)
})

test_that("reference_comparisons supports nlme::lme models", {
	skip_if_not_installed("nlme")
	# A common (scalar) df, so the exact Dunnett test is used.
	m <- nlme::lme(
		distance ~ Sex,
		random = ~ 1 | Subject,
		data = nlme::Orthodont
	)
	out <- reference_comparisons(m, classify = "Sex", reference = "Male")
	expect_s3_class(out, "reference_comparisons")
	expect_equal(nrow(out), 1L) # 2 levels -> 1 comparison vs the reference
	expect_equal(attr(out, "comparison_method"), "dunnett")
	expect_true(is.finite(out$df))
})

test_that("reference_comparisons supports lme4::lmer models (Holm fallback)", {
	skip_if_not_installed("lme4")
	# lmer reports comparison-specific (Kenward-Roger) df, so exact Dunnett is
	# unavailable and the adjustment falls back to Holm with a warning.
	m <- lme4::lmer(weight ~ Diet + (1 | Chick), data = ChickWeight)
	expect_warning(
		out <- reference_comparisons(m, classify = "Diet", reference = "1"),
		"Falling back"
	)
	expect_s3_class(out, "reference_comparisons")
	expect_equal(nrow(out), 3L) # 4 diets -> 3 comparisons vs the reference
	expect_equal(attr(out, "comparison_method"), "holm")
})

test_that("descending orders rows by estimate within the group", {
	m <- aov(weight ~ feed, data = chickwts)
	asc <- reference_comparisons(
		m,
		classify = "feed",
		reference = "casein",
		descending = FALSE
	)
	expect_equal(asc$estimate, sort(asc$estimate))
	desc <- reference_comparisons(
		m,
		classify = "feed",
		reference = "casein",
		descending = TRUE
	)
	expect_equal(desc$estimate, sort(desc$estimate, decreasing = TRUE))
})

test_that("print shows the reference header", {
	m <- aov(weight ~ feed, data = chickwts)
	out <- reference_comparisons(m, classify = "feed", reference = "casein")
	expect_output(print(out), "Comparisons against a reference level")
	expect_output(print(out), "Reference: casein")
})

test_that("autoplot returns a ggplot with the expected layers", {
	m <- aov(weight ~ feed, data = chickwts)
	out <- reference_comparisons(m, classify = "feed", reference = "casein")
	p <- ggplot2::autoplot(out)
	expect_s3_class(p, "ggplot")
	geoms <- vapply(p$layers, function(l) class(l$geom)[1], character(1))
	expect_true("GeomVline" %in% geoms)
	expect_true("GeomLinerange" %in% geoms)
	expect_true("GeomPoint" %in% geoms)
})

test_that("autoplot reference means plot is stable", {
	skip_if_not_installed("vdiffr")
	m <- aov(weight ~ feed, data = chickwts)
	# Use a deterministic adjustment for the snapshot: the default Dunnett uses
	# Monte-Carlo qmvt for the interval widths, so its SVG is not reproducible.
	# The Dunnett numbers are validated against emmeans in a separate test.
	out <- suppressMessages(reference_comparisons(
		m,
		classify = "feed",
		reference = "casein",
		adjust = "holm"
	))
	vdiffr::expect_doppelganger(
		"reference means plot",
		ggplot2::autoplot(out),
		variant = ggplot2_variant()
	)
})

test_that("reference_comparisons supports asreml models", {
	# asreml is commercial and unlicensed on CI/CRAN, so this is skipped there
	# and only runs on a licensed machine (run manually to verify).
	skip_on_cran()
	skip_if_not_installed("asreml")
	quiet(library(asreml))
	# Load into the global environment: asreml's predict()/wald() refit the model
	# by evaluating its original call, which needs the data object on the search
	# path (matching how test-all-w2.r loads these fixtures).
	suppressWarnings(load(
		test_path("data", "w2_models.Rdata"),
		envir = .GlobalEnv
	))

	out <- reference_comparisons(
		example3.asr,
		classify = "Variety",
		reference = "Excell"
	)
	expect_s3_class(out, "reference_comparisons")
	expect_equal(nrow(out), 3L) # 4 varieties -> 3 comparisons vs the reference
	expect_true(all(out$level2 == "Excell"))
	# asreml reports a single (scalar) denDF, so the exact Dunnett test is used.
	expect_equal(attr(out, "comparison_method"), "dunnett")
})

test_that("get_predictions returns asreml's exact prediction vcov", {
	# The comparison functions take the variance-covariance of the predicted
	# means straight from asreml's predict(..., vcov = TRUE); this checks that
	# get_predictions() surfaces that matrix unchanged (and consistent with the
	# reported SEs).
	#
	# asreml is commercial and unlicensed on CI/CRAN, so this is skipped there and
	# only runs on a licensed machine.
	skip_on_cran()
	skip_on_ci()
	skip_if_not_installed("asreml")
	quiet(library(asreml))

	# Load into the global environment: asreml's predict()/wald() refit the model
	# by evaluating its original call, which needs the data object on the search
	# path (matching how test-all-w2.r loads these fixtures).
	suppressWarnings(load(
		test_path("data", "w2_models.Rdata"),
		envir = .GlobalEnv
	))
	classify <- "Variety"

	# Native vcov of the predicted means straight from asreml.
	pred <- predict(example3.asr, classify = classify, vcov = TRUE)
	V_direct <- as.matrix(pred$vcov)

	# The vcov surfaced by get_predictions().
	res <- biometryassist:::get_predictions(example3.asr, classify)
	expect_equal(unname(as.matrix(res$vcov)), unname(V_direct), tolerance = 1e-8)
	# Diagonal of V matches the reported standard errors squared.
	expect_equal(
		sqrt(diag(as.matrix(res$vcov))),
		res$predictions$std.error,
		tolerance = 1e-6
	)
})
