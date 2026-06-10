test_that("pairwise_comparisons returns a tidy data frame with expected columns", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- pairwise_comparisons(dat.aov, classify = "Species")

	expect_s3_class(out, "pairwise_comparisons")
	expect_s3_class(out, "data.frame")
	expect_true(all(
		c(
			"level1",
			"level2",
			"comparison",
			"estimate",
			"std.error",
			"statistic",
			"df",
			"p.value",
			"conf.low",
			"conf.high"
		) %in%
			names(out)
	))
	# All pairs of a 3-level factor => choose(3, 2) rows
	expect_equal(nrow(out), 3)
	expect_equal(attr(out, "comparison_method"), "holm")
	expect_equal(attr(out, "classify"), "Species")
})

test_that("estimates, SEs and p-values match an independent emmeans contrast", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- pairwise_comparisons(dat.aov, classify = "Species", adjust = "none")

	ref <- as.data.frame(emmeans::contrast(
		emmeans::emmeans(dat.aov, ~Species),
		"pairwise",
		adjust = "none"
	))
	# emmeans labels e.g. "setosa - versicolor"; align on comparison label
	ref_est <- stats::setNames(ref$estimate, gsub(" ", "", ref$contrast))
	our_est <- stats::setNames(out$estimate, gsub(" ", "", out$comparison))

	expect_equal(
		our_est[names(ref_est)],
		ref_est,
		tolerance = 1e-6
	)
	ref_se <- stats::setNames(ref$SE, gsub(" ", "", ref$contrast))
	our_se <- stats::setNames(out$std.error, gsub(" ", "", out$comparison))
	expect_equal(our_se[names(ref_se)], ref_se, tolerance = 1e-6)

	ref_p <- stats::setNames(ref$p.value, gsub(" ", "", ref$contrast))
	our_p <- stats::setNames(out$p.value, gsub(" ", "", out$comparison))
	expect_equal(our_p[names(ref_p)], ref_p, tolerance = 1e-6)
})

test_that("sign convention follows the order written", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	fwd <- pairwise_comparisons(
		dat.aov,
		classify = "Species",
		pairs = "setosa-virginica"
	)
	rev <- pairwise_comparisons(
		dat.aov,
		classify = "Species",
		pairs = "virginica-setosa"
	)
	expect_equal(fwd$estimate, -rev$estimate)
	expect_equal(fwd$level1, "setosa")
	expect_equal(rev$level1, "virginica")
})

test_that("adjust methods behave as documented", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	raw <- pairwise_comparisons(dat.aov, classify = "Species", adjust = "none")
	bonf <- pairwise_comparisons(
		dat.aov,
		classify = "Species",
		adjust = "bonferroni"
	)

	m <- nrow(raw)
	expect_equal(bonf$p.value, pmin(raw$p.value * m, 1))

	expect_error(
		pairwise_comparisons(dat.aov, classify = "Species", adjust = "tukey"),
		"multiple_comparisons"
	)
	expect_error(
		pairwise_comparisons(dat.aov, classify = "Species", adjust = "nonsense"),
		"Invalid `adjust`"
	)
})

test_that("pairs = NULL expands to all pairwise comparisons", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- pairwise_comparisons(dat.aov, classify = "Species", pairs = NULL)
	expect_equal(nrow(out), choose(3, 2))
})

test_that("descending tri-state orders rows within group", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	p <- c("setosa-versicolor", "setosa-virginica", "versicolor-virginica")

	input_order <- pairwise_comparisons(dat.aov, classify = "Species", pairs = p)
	expect_equal(input_order$comparison, gsub("-", " - ", p))

	asc <- pairwise_comparisons(
		dat.aov,
		classify = "Species",
		pairs = p,
		descending = FALSE
	)
	expect_false(is.unsorted(asc$estimate))

	desc <- pairwise_comparisons(
		dat.aov,
		classify = "Species",
		pairs = p,
		descending = TRUE
	)
	expect_false(is.unsorted(rev(desc$estimate)))
})

test_that("duplicate / reversed pairs warn and de-duplicate", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_warning(
		out <- pairwise_comparisons(
			dat.aov,
			classify = "Species",
			pairs = c("setosa-versicolor", "versicolor-setosa")
		),
		"Duplicate or reversed"
	)
	expect_equal(nrow(out), 1)
})

test_that("unknown levels and identical-level pairs error clearly", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_error(
		pairwise_comparisons(
			dat.aov,
			classify = "Species",
			pairs = "setosa-nope"
		),
		"Unknown level"
	)
	expect_error(
		pairwise_comparisons(
			dat.aov,
			classify = "Species",
			pairs = "setosa-setosa"
		),
		"two distinct levels"
	)
})

test_that("interaction classify: string and list forms are equivalent", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d), mean = as.numeric(d$Trt) + ifelse(d$Grp == "G2", 5, 0))
	m <- aov(y ~ Trt * Grp, data = d)

	str_form <- pairwise_comparisons(
		m,
		classify = "Trt:Grp",
		pairs = "A:G1-B:G1"
	)
	list_form <- pairwise_comparisons(
		m,
		classify = "Trt:Grp",
		pairs = list(c("A:G1", "B:G1"))
	)
	expect_equal(str_form$estimate, list_form$estimate)
	expect_equal(str_form$comparison, "A:G1 - B:G1")
})

test_that("by splits comparisons and adjusts within each group", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d), mean = as.numeric(d$Trt) + ifelse(d$Grp == "G2", 5, 0))
	m <- aov(y ~ Trt * Grp, data = d)

	out <- pairwise_comparisons(m, classify = "Trt:Grp", by = "Grp")
	# 3 Trt pairs within each of 2 groups
	expect_equal(nrow(out), 2 * choose(3, 2))
	expect_true("Grp" %in% names(out))
	expect_setequal(as.character(unique(out$Grp)), c("G1", "G2"))
	# within-group labels reference only the remaining (Trt) factor
	expect_true(all(out$level1 %in% c("A", "B", "C")))

	# by consuming all classify factors errors
	expect_error(
		pairwise_comparisons(m, classify = "Trt", by = "Trt"),
		"consumes all"
	)
})

test_that("levels containing '-' require the list form", {
	set.seed(2)
	d <- data.frame(
		Trt = factor(rep(c("lo-fat", "hi-fat", "mid"), each = 8)),
		y = rnorm(24)
	)
	m <- aov(y ~ Trt, data = d)

	# string form cannot disambiguate the hyphen in the level name
	expect_error(
		pairwise_comparisons(m, classify = "Trt", pairs = "lo-fat-hi-fat"),
		"list form"
	)
	# list form is unambiguous
	out <- pairwise_comparisons(
		m,
		classify = "Trt",
		pairs = list(c("lo-fat", "hi-fat"))
	)
	expect_equal(nrow(out), 1)
	expect_equal(out$level1, "lo-fat")
})

test_that("print method shows a header and the table", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- pairwise_comparisons(dat.aov, classify = "Species")
	expect_output(print(out), "Pairwise comparisons of means")
	expect_output(print(out), "Adjustment method: holm")
})

test_that("autoplot builds the expected forest-plot layers", {
	has_geom <- function(p, cls) {
		any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
	}

	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	pc <- pairwise_comparisons(dat.aov, classify = "Species")
	p <- autoplot(pc)

	expect_s3_class(p, "ggplot")
	expect_true(has_geom(p, "GeomVline")) # zero reference line
	expect_true(has_geom(p, "GeomLinerange")) # confidence intervals
	expect_true(has_geom(p, "GeomPoint")) # estimates
	expect_match(p$labels$x, "Estimated difference")
	# builds without error/warning
	expect_silent(ggplot2::ggplot_build(p))
})

test_that("autoplot flags significant comparisons with an asterisk", {
	has_geom <- function(p, cls) {
		any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
	}

	# Unfaceted: significant comparisons get a "*" on their y-axis label.
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	p <- autoplot(pairwise_comparisons(dat.aov, classify = "Species"))
	ylabs <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]$y$get_labels()
	# significant comparisons are prefixed with "* " (keeps labels right-justified)
	expect_true(any(grepl("^\\* ", ylabs)))
	# no colour legend / significance aesthetic any more
	expect_false(has_geom(p, "GeomText")) # unfaceted uses axis labels, not text

	# Faceted: significance is marked beside the interval (a text layer).
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d), mean = as.numeric(d$Trt) + ifelse(d$Grp == "G2", 5, 0))
	m <- aov(y ~ Trt * Grp, data = d)
	pf <- autoplot(pairwise_comparisons(m, classify = "Trt:Grp", by = "Grp"))
	expect_true(has_geom(pf, "GeomText"))
})

test_that("autoplot facets by the `by` variable", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d), mean = as.numeric(d$Trt) + ifelse(d$Grp == "G2", 5, 0))
	m <- aov(y ~ Trt * Grp, data = d)
	pc <- pairwise_comparisons(m, classify = "Trt:Grp", by = "Grp")

	p <- autoplot(pc)
	expect_true("Grp" %in% names(p$facet$params$facets))
})

test_that("autoplot forest plot matches its snapshot", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	pc <- pairwise_comparisons(dat.aov, classify = "Species")
	vdiffr::expect_doppelganger(
		"pairwise forest plot",
		autoplot(pc),
		variant = ggplot2_variant()
	)
})
