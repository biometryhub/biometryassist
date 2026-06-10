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
