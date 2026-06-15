dat.aov <- aov(Petal.Width ~ Species, data = iris)

# ============================================================================
# NEW TESTS FOR LIST STRUCTURE AND P-VALUE MATRIX
# ============================================================================

test_that("multiple_comparisons returns a list with correct structure", {
	output <- multiple_comparisons(dat.aov, classify = "Species")

	# Test that output is a list
	expect_type(output, "list")
	expect_s3_class(output, "mct")

	# Test that all expected elements are present
	expect_true("predictions" %in% names(output))
	expect_true("pairwise_pvalues" %in% names(output))
	expect_true("hsd" %in% names(output))
	expect_true("sig_level" %in% names(output))

	# Test that each element has the correct type
	expect_s3_class(output$predictions, "data.frame")
	expect_type(output$pairwise_pvalues, "double")
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_type(output$hsd, "double")
	expect_type(output$sig_level, "double")
})

test_that("predictions component has correct structure", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	pred <- output$predictions

	# Check it's a data frame
	expect_s3_class(pred, "data.frame")

	# Check expected columns exist
	expect_true("Species" %in% colnames(pred))
	expect_true("predicted.value" %in% colnames(pred))
	expect_true("std.error" %in% colnames(pred))
	expect_true("low" %in% colnames(pred))
	expect_true("up" %in% colnames(pred))
	expect_true("groups" %in% colnames(pred))

	# Check number of rows
	expect_equal(nrow(pred), 3) # 3 species

	# Check values are correct
	expect_equal(pred$predicted.value, c(0.25, 1.33, 2.03), tolerance = 5e-2)
})

test_that("p-value matrix has correct properties", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	pvals <- output$pairwise_pvalues

	# Check it's a matrix
	expect_true(is.matrix(pvals))

	# Check dimensions
	expect_equal(nrow(pvals), 3)
	expect_equal(ncol(pvals), 3)

	# Check row and column names
	expect_equal(rownames(pvals), c("setosa", "versicolor", "virginica"))
	expect_equal(colnames(pvals), c("setosa", "versicolor", "virginica"))

	# Check diagonal is 1 (no difference with itself)
	expect_equal(diag(pvals), c(setosa = 1, versicolor = 1, virginica = 1))

	# Check all values are in [0, 1]
	expect_true(all(pvals >= 0 & pvals <= 1))

	# Check symmetry
	expect_true(isSymmetric(pvals))

	# Check that p-values match expected significance
	# setosa vs versicolor should be significant (different groups)
	expect_true(pvals["setosa", "versicolor"] < 0.05)
	# setosa vs virginica should be significant
	expect_true(pvals["setosa", "virginica"] < 0.05)
	# versicolor vs virginica should be significant
	expect_true(pvals["versicolor", "virginica"] < 0.05)
})

test_that("calculate_pvalue_matrix supports Matrix SED inputs", {
	skip_if_not_installed("Matrix")

	pp <- data.frame(
		predicted.value = c(10, 12, 13, 15),
		Names = c("A", "B", "C", "D")
	)

	sed_base <- matrix(0.5, nrow = 4, ncol = 4)
	diag(sed_base) <- NA_real_

	# Create a packed symmetric *dense* Matrix (dspMatrix).
	# Using sparse=TRUE yields a dsCMatrix which cannot be coerced to dspMatrix.
	sed_mat <- as(
		Matrix::forceSymmetric(
			Matrix::Matrix(sed_base, sparse = FALSE),
			uplo = "U"
		),
		"packedMatrix"
	)

	pvals <- biometryassist:::calculate_pvalue_matrix(pp, sed_mat, ndf = 10)

	expect_true(is.matrix(pvals))
	expect_type(pvals, "double")
	expect_true(isSymmetric(pvals))
	expect_equal(rownames(pvals), pp$Names)
	expect_equal(colnames(pvals), pp$Names)
	expect_true(all(
		pvals[upper.tri(pvals)] >= 0 & pvals[upper.tri(pvals)] <= 1,
		na.rm = TRUE
	))
})

test_that("calculate_pvalue_matrix supports scalar SED inputs", {
	pp <- data.frame(
		predicted.value = c(10, 12, 13, 15),
		Names = c("A", "B", "C", "D")
	)

	# Scalar SED implies the same standard error for every comparison.
	# This covers the `rep_len(sed, length(diff))` branch.
	pvals <- biometryassist:::calculate_pvalue_matrix(pp, sed = 0.5, ndf = 10)

	expect_true(is.matrix(pvals))
	expect_type(pvals, "double")
	expect_true(isSymmetric(pvals))
	expect_equal(rownames(pvals), pp$Names)
	expect_equal(colnames(pvals), pp$Names)
	expect_equal(diag(pvals), rep(1, nrow(pp)), ignore_attr = TRUE)
	expect_true(all(
		pvals[upper.tri(pvals)] >= 0 & pvals[upper.tri(pvals)] <= 1,
		na.rm = TRUE
	))
})

test_that("calculate_pvalue_matrix supports base-matrix SED inputs", {
	pp <- data.frame(
		predicted.value = c(10, 11, 15),
		Names = c("A", "B", "C")
	)

	# A plain base matrix should take the `!is.null(dim(sed))` branch.
	sed_mat <- matrix(
		0.5,
		nrow = 3,
		ncol = 3,
		dimnames = list(pp$Names, pp$Names)
	)
	diag(sed_mat) <- NA_real_

	pvals <- biometryassist:::calculate_pvalue_matrix(
		pp,
		sed = sed_mat,
		ndf = 10
	)

	expect_true(is.matrix(pvals))
	expect_type(pvals, "double")
	expect_true(isSymmetric(pvals))
	expect_equal(rownames(pvals), pp$Names)
	expect_equal(colnames(pvals), pp$Names)
	expect_equal(diag(pvals), rep(1, 3), ignore_attr = TRUE)

	# Spot-check one expected p-value to ensure indexing is correct.
	diff_ab <- abs(pp$predicted.value[1] - pp$predicted.value[2])
	q_ab <- as.numeric(diff_ab / 0.5) * sqrt(2)
	expected_ab <- stats::ptukey(q_ab, nmeans = 3, df = 10, lower.tail = FALSE)
	expect_equal(pvals["A", "B"], expected_ab, tolerance = 1e-12)
})

test_that("calculate_pvalue_matrix returns a 1x1 matrix when n <= 1", {
	pp <- data.frame(
		predicted.value = 10,
		Names = "A"
	)

	pvals <- biometryassist:::calculate_pvalue_matrix(pp, sed = 0.5, ndf = 10)

	expect_true(is.matrix(pvals))
	expect_equal(dim(pvals), c(1L, 1L))
	expect_equal(rownames(pvals), "A")
	expect_equal(colnames(pvals), "A")
	expect_equal(pvals[1, 1], 1)
})

test_that("HSD value is accessible and correct", {
	output <- multiple_comparisons(dat.aov, classify = "Species")

	# Check HSD is numeric
	expect_type(output$hsd, "double")

	# Check HSD is positive
	expect_true(output$hsd > 0)

	# Check HSD is accessible via attribute for backward compatibility
	expect_equal(attr(output, "HSD"), output$hsd)
})

test_that("sig_level is stored correctly", {
	output1 <- multiple_comparisons(dat.aov, classify = "Species", sig = 0.05)
	expect_equal(output1$sig_level, 0.05)

	output2 <- multiple_comparisons(dat.aov, classify = "Species", sig = 0.01)
	expect_equal(output2$sig_level, 0.01)
})

test_that("p-value matrix calculation with interaction terms", {
	# Create data with two factors
	set.seed(123)
	factorial_data <- expand.grid(
		Factor1 = factor(c("A", "B")),
		Factor2 = factor(c("X", "Y")),
		Rep = 1:5
	)
	factorial_data$Response <- rnorm(
		nrow(factorial_data),
		mean = 50 +
			as.numeric(factorial_data$Factor1) * 10 +
			as.numeric(factorial_data$Factor2) * 5,
		sd = 5
	)

	model_int <- aov(Response ~ Factor1 * Factor2, data = factorial_data)
	output <- multiple_comparisons(model_int, classify = "Factor1:Factor2")

	# Check matrix dimensions (2 * 2 = 4 combinations)
	expect_equal(nrow(output$pairwise_pvalues), 4)
	expect_equal(ncol(output$pairwise_pvalues), 4)

	# Check row/column names are treatment combinations
	expected_names <- c("A_X", "A_Y", "B_X", "B_Y")
	expect_equal(sort(rownames(output$pairwise_pvalues)), sort(expected_names))

	# Check matrix properties
	expect_true(isSymmetric(output$pairwise_pvalues))
	expect_equal(diag(output$pairwise_pvalues), rep(1, 4), ignore_attr = TRUE)
})

test_that("p-values without letter groups still calculated", {
	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		groups = FALSE
	)

	# Check predictions doesn't have groups column
	expect_false("groups" %in% colnames(output$predictions))

	# Check p-value matrix is still created
	expect_true("pairwise_pvalues" %in% names(output))
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_equal(nrow(output$pairwise_pvalues), 3)
})

test_that("print.mct shows list structure information", {
	output <- multiple_comparisons(dat.aov, classify = "Species")

	# Capture print output
	printed <- capture.output(print(output))

	# Check for key elements in output
	expect_true(any(grepl("Multiple Comparisons", printed)))
	expect_true(any(grepl("Significance level", printed)))
	expect_true(any(grepl("HSD", printed)))
	expect_true(any(grepl("Predicted values", printed)))
})

test_that("aliased treatments in list structure", {
	CO_2 <- CO2
	CO_2$uptake[CO_2$Type == "Quebec" & CO_2$Treatment == "nonchilled"] <- NA
	model <- aov(uptake ~ Type + Treatment + Type:Treatment, data = CO_2)

	expect_warning(
		output <- multiple_comparisons(model, classify = "Type:Treatment"),
		"A level of Type\\:Treatment is aliased"
	)

	# Check aliased is stored in list
	expect_true("aliased" %in% names(output))
	expect_equal(output$aliased, "Quebec:nonchilled")

	# Check aliased is also in attributes for backward compatibility
	expect_equal(attr(output, "aliased"), "Quebec:nonchilled")
})

test_that("backward compatibility: attributes still accessible", {
	output <- multiple_comparisons(dat.aov, classify = "Species")

	# Check attributes that should be preserved
	expect_false(is.null(attr(output, "ylab")))
	expect_false(is.null(attr(output, "HSD")))
	expect_equal(attr(output, "HSD"), output$hsd)
})

test_that("list elements accessible via $ and [[", {
	output <- multiple_comparisons(dat.aov, classify = "Species")

	# Test $ access
	expect_s3_class(output$predictions, "data.frame")
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_type(output$hsd, "double")
	expect_type(output$sig_level, "double")

	# Test [[ access
	expect_identical(output$predictions, output[[1]])
	expect_identical(output$pairwise_pvalues, output[[2]])
	expect_identical(output$hsd, output[[3]])
	expect_identical(output$sig_level, output[[4]])
})

test_that("p-value matrix with different interval types", {
	# All interval types should produce the same p-value matrix
	output_ci <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		int.type = "ci"
	)
	output_tukey <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		int.type = "tukey"
	)
	output_1se <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		int.type = "1se"
	)

	# P-values should be identical regardless of interval type
	expect_equal(output_ci$pairwise_pvalues, output_tukey$pairwise_pvalues)
	expect_equal(output_ci$pairwise_pvalues, output_1se$pairwise_pvalues)
})

test_that("p-value matrix with transformations", {
	dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
	output <- multiple_comparisons(
		dat.aov.log,
		classify = "Species",
		trans = "log",
		offset = 0
	)

	# Check p-value matrix exists and has correct properties
	expect_true("pairwise_pvalues" %in% names(output))
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_equal(nrow(output$pairwise_pvalues), 3)
	expect_true(isSymmetric(output$pairwise_pvalues))
	expect_equal(diag(output$pairwise_pvalues), rep(1, 3), ignore_attr = TRUE)
})


# ============================================================================
# TESTS FOR adjust / by ARGUMENTS
# ============================================================================

test_that("default adjust is tukey and adds comparison_method", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	expect_equal(output$comparison_method, "tukey")
	expect_type(output$hsd, "double")
	# Only a single p-value object is returned
	expect_false("raw_pvalues" %in% names(output))
	expect_true("pairwise_pvalues" %in% names(output))
	# Backwards-compatible positional access preserved
	expect_identical(output$predictions, output[[1]])
	expect_identical(output$pairwise_pvalues, output[[2]])
	expect_identical(output$hsd, output[[3]])
	expect_identical(output$sig_level, output[[4]])
})

test_that("invalid adjust method errors", {
	expect_error(
		multiple_comparisons(dat.aov, classify = "Species", adjust = "nope"),
		"Invalid `adjust` method"
	)
})

test_that("non-tukey adjust sets hsd NULL and returns a single p-value matrix", {
	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		adjust = "bonferroni"
	)
	expect_equal(output$comparison_method, "bonferroni")
	expect_null(output$hsd)
	# A single adjusted p-value object, no separate raw_pvalues
	expect_false("raw_pvalues" %in% names(output))
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_true(isSymmetric(output$pairwise_pvalues))
	expect_equal(diag(output$pairwise_pvalues), rep(1, 3), ignore_attr = TRUE)
})

test_that("adjust does not double-adjust p-values", {
	# Build data with non-trivial (non-zero) p-values
	set.seed(42)
	d <- data.frame(
		Trt = factor(rep(c("A", "B", "C", "D"), each = 8)),
		y = rnorm(32)
	)
	d$y[d$Trt == "A"] <- d$y[d$Trt == "A"] + 1.2
	m <- aov(y ~ Trt, data = d)

	out_bonf <- multiple_comparisons(m, classify = "Trt", adjust = "bonferroni")
	out_none <- multiple_comparisons(m, classify = "Trt", adjust = "none")

	# adjust = "none" returns the raw (unadjusted) p-values, so Bonferroni
	# should equal those raw values * (number of comparisons), capped at 1.
	raw <- out_none$pairwise_pvalues
	adj <- out_bonf$pairwise_pvalues
	manual <- pmin(raw * choose(4, 2), 1)
	expect_equal(adj[lower.tri(adj)], manual[lower.tri(manual)])
})

test_that("adjust = none returns raw two-sided t-test p-values", {
	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		adjust = "none"
	)
	expect_true(is.matrix(output$pairwise_pvalues))
	expect_equal(diag(output$pairwise_pvalues), rep(1, 3), ignore_attr = TRUE)
	expect_true(isSymmetric(output$pairwise_pvalues))
})

test_that("by splits comparisons into per-group families", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(
		nrow(d),
		mean = as.numeric(d$Trt) * ifelse(d$Grp == "G1", 5, 1)
	)
	m <- aov(y ~ Trt * Grp, data = d)

	output <- multiple_comparisons(m, classify = "Trt:Grp", by = "Grp")

	# pairwise_pvalues and hsd are per-group lists
	expect_type(output$pairwise_pvalues, "list")
	expect_equal(sort(names(output$pairwise_pvalues)), c("G1", "G2"))
	expect_type(output$hsd, "list")

	# Each group's matrix only contains that group's treatments
	expect_equal(
		sort(rownames(output$pairwise_pvalues$G1)),
		c("A_G1", "B_G1", "C_G1")
	)

	# Letter groupings restart within each group
	expect_true("groups" %in% names(output$predictions))

	# `by` is recorded as an attribute for autoplot faceting
	expect_equal(attr(output, "by"), "Grp")
})

test_that("by uses per-group nmeans for Tukey comparison intervals", {
	# Regression: with `by`, `int.type = "tukey"` intervals must use each
	# subgroup's mean count (here 3), not the total across subgroups (6), so the
	# intervals stay consistent with the per-group letter groupings.
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d), mean = as.numeric(d$Trt))
	m <- aov(y ~ Trt * Grp, data = d)

	output <- multiple_comparisons(
		m,
		classify = "Trt:Grp",
		by = "Grp",
		int.type = "tukey"
	)

	df_res <- m$df.residual
	se1 <- output$predictions$std.error[1]
	ci_pergroup <- qtukey(0.95, nmeans = 3, df = df_res) / sqrt(2) * se1
	ci_total <- qtukey(0.95, nmeans = 6, df = df_res) / sqrt(2) * se1

	expect_equal(output$predictions$ci[1], ci_pergroup)
	expect_false(isTRUE(all.equal(ci_pergroup, ci_total)))
})

test_that("check_ci_consistency detects shared letters with disjoint CIs", {
	# Detection-only helper: TRUE iff a non-overlapping CI pair shares a letter.
	inconsistent <- data.frame(
		predicted.value = c(0, 5),
		ci = c(1, 1), # CIs [-1,1] and [4,6] do not overlap ...
		groups = c("a", "a") # ... yet they share letter "a"
	)
	expect_true(check_ci_consistency(inconsistent))

	consistent <- data.frame(
		predicted.value = c(0, 5),
		ci = c(1, 1),
		groups = c("a", "b") # disjoint CIs, different letters -> no conflict
	)
	expect_false(check_ci_consistency(consistent))

	overlapping <- data.frame(
		predicted.value = c(0, 1),
		ci = c(1, 1), # CIs [-1,1] and [0,2] overlap
		groups = c("a", "a")
	)
	expect_false(check_ci_consistency(overlapping))
})

test_that("CI/letter inconsistency note is emitted under `by`", {
	# Regression: the consistency note must also fire for grouped comparisons,
	# not only the single-group case. The note can only arise with enough means
	# per group that the regular-CI half-widths sum to less than the HSD, so use
	# 10 levels with deterministic, exactly-spaced means (zero-sum residuals give
	# exact cell means and a fixed residual SD, so this is not a flaky knife-edge).
	k <- 10
	n <- 8
	df <- 2 * k * (n - 1)
	# Place adjacent means midway through the window
	# (2*qt(0.975) < gap/se < qtukey) so adjacent pairs share a letter yet have
	# non-overlapping regular CIs.
	gap <- (2 * qt(0.975, df) + qtukey(0.95, k, df)) / 2
	sigma <- sqrt(n) # makes the per-cell std error equal to 1
	resid <- scale(seq_len(n), center = TRUE, scale = FALSE)[, 1]
	resid <- resid / sd(resid) * sigma

	d <- data.frame(
		Trt = factor(rep(LETTERS[1:k], each = n * 2)),
		Grp = factor(rep(rep(c("G1", "G2"), each = n), times = k)),
		y = rep((0:(k - 1)) * gap, each = n * 2) + rep(resid, times = k * 2)
	)
	m <- aov(y ~ Trt * Grp, data = d)

	expect_message(
		multiple_comparisons(m, classify = "Trt:Grp", by = "Grp", int.type = "ci"),
		"non-overlapping confidence intervals"
	)
})

test_that("by with a missing column errors", {
	expect_error(
		multiple_comparisons(dat.aov, classify = "Species", by = "NotAColumn"),
		"are not present in the predictions"
	)
})

test_that("by with a single-factor classify errors", {
	# Only one factor in classify leaves nothing to compare within groups
	expect_error(
		multiple_comparisons(dat.aov, classify = "Species", by = "Species"),
		"cannot include all of the `classify` variable"
	)
})

test_that("by errors when it consumes all classify factors", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(nrow(d))
	m <- aov(y ~ Trt * Grp, data = d)

	expect_error(
		multiple_comparisons(m, classify = "Trt:Grp", by = c("Trt", "Grp")),
		"cannot include all of the `classify` variable"
	)
})

test_that("autoplot facets by the `by` variable", {
	set.seed(1)
	d <- expand.grid(
		Trt = factor(c("A", "B", "C")),
		Grp = factor(c("G1", "G2")),
		rep = 1:6
	)
	d$y <- rnorm(
		nrow(d),
		mean = as.numeric(d$Trt) * ifelse(d$Grp == "G1", 5, 1)
	)
	m <- aov(y ~ Trt * Grp, data = d)
	output <- multiple_comparisons(m, classify = "Trt:Grp", by = "Grp")

	p <- autoplot(output)
	facet_vars <- names(p$facet$params$facets)
	expect_true("Grp" %in% facet_vars)
	expect_false("Trt" %in% facet_vars) # Trt is the x-axis, not a facet
})

test_that("autoplot.mct type, errorbar and lettering options behave", {
	has_geom <- function(p, cls) {
		any(vapply(p$layers, function(l) inherits(l$geom, cls), logical(1)))
	}

	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- multiple_comparisons(dat.aov, classify = "Species")

	# type = "line" adds a line layer; "bar" is an alias for a column graph
	expect_true(has_geom(autoplot(out, type = "line"), "GeomLine"))
	expect_true(has_geom(autoplot(out, type = "bar"), "GeomCol"))

	# Toggles remove the relevant layers
	expect_false(has_geom(
		autoplot(out, include_errorbar = FALSE),
		"GeomErrorbar"
	))
	expect_false(has_geom(autoplot(out, include_lettering = FALSE), "GeomText"))

	# Invalid type errors cleanly rather than silently producing an empty plot
	expect_error(autoplot(out, type = "wiggle"), "should be one of")
})

test_that("autoplot.mct HSD reference bar works without a transformation", {
	# Regression: requesting an HSD bar on an untransformed model previously
	# errored via a forced back-transformation path.
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- multiple_comparisons(dat.aov, classify = "Species")

	p <- autoplot(out, errorbar_type = "hsd")
	expect_s3_class(p, "ggplot")
	expect_silent(ggplot2::ggplot_build(p))

	# The HSD bar is a single reference bar, so its layer carries one row of data
	eb <- Filter(function(l) inherits(l$geom, "GeomErrorbar"), p$layers)
	expect_length(eb, 1)
	expect_equal(nrow(eb[[1]]$data), 1)
})

test_that("autoplot.mct errors for an HSD bar when no single HSD is available", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	# Non-Tukey adjustment => $hsd is NULL
	out <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		adjust = "bonferroni"
	)
	expect_error(
		autoplot(out, errorbar_type = "hsd"),
		"Honest Significant"
	)
})

test_that("autoplot.mct adds a back-transformed secondary axis on the model scale", {
	has_sec_axis <- function(p) {
		any(vapply(
			p$scales$scales,
			function(s) inherits(s$secondary.axis, "AxisSecondary"),
			logical(1)
		))
	}

	m_log <- aov(log(Petal.Width) ~ Species, data = iris)
	out_log <- multiple_comparisons(
		m_log,
		classify = "Species",
		trans = "log",
		offset = 0
	)

	# Default plots on the (interpretable) back-transformed scale: no second axis
	expect_false(has_sec_axis(autoplot(out_log)))
	# Model scale shows the back-transformed secondary axis
	expect_true(has_sec_axis(autoplot(out_log, trans_scale = TRUE)))
	# HSD forces the model scale, so it also gets the secondary axis
	expect_true(has_sec_axis(autoplot(out_log, errorbar_type = "hsd")))

	# The transform metadata is recorded on the object for the axis
	expect_equal(attr(out_log, "trans"), "log")
	expect_equal(attr(out_log, "offset"), 0)
})

test_that("back_transform is the inverse used to build PredictedValue", {
	x <- c(0.5, 1, 1.5, 2)
	expect_equal(biometryassist:::back_transform(x, "log", 0), exp(x))
	expect_equal(biometryassist:::back_transform(x, "sqrt", 0), x^2)
	expect_equal(biometryassist:::back_transform(x, "power", 0, 2), x^(1 / 2))
	expect_equal(biometryassist:::back_transform(x, "inverse"), 1 / x)
	expect_error(biometryassist:::back_transform(x, "nonsense"), "Invalid trans")
})

test_that("autoplot.mct new options render as expected", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	out <- multiple_comparisons(dat.aov, classify = "Species")

	expect_local_doppelganger("mct line plot", autoplot(out, type = "line"))
	expect_local_doppelganger(
		"mct hsd bar",
		autoplot(out, errorbar_type = "hsd")
	)
	expect_local_doppelganger(
		"mct no errorbar",
		autoplot(out, include_errorbar = FALSE)
	)
	expect_local_doppelganger(
		"mct no lettering",
		autoplot(out, include_lettering = FALSE)
	)
})

test_that("autoplot.mct transformed-scale options render as expected", {
	dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
	output.log <- multiple_comparisons(
		dat.aov.log,
		classify = "Species",
		trans = "log",
		offset = 0
	)

	# Model scale with the back-transformed secondary axis
	expect_local_doppelganger(
		"mct log transformed scale",
		autoplot(output.log, trans_scale = TRUE)
	)
	# HSD bar forces the model scale and adds the secondary axis
	expect_local_doppelganger(
		"mct log hsd bar",
		autoplot(output.log, errorbar_type = "hsd")
	)
})

test_that("calculate_raw_pvalue_matrix matches a direct t-test", {
	pp <- data.frame(
		predicted.value = c(10, 11, 15),
		Names = c("A", "B", "C")
	)
	sed_mat <- matrix(
		0.5,
		nrow = 3,
		ncol = 3,
		dimnames = list(pp$Names, pp$Names)
	)
	diag(sed_mat) <- NA_real_

	pvals <- biometryassist:::calculate_raw_pvalue_matrix(pp, sed_mat, ndf = 10)

	expect_true(isSymmetric(pvals))
	expect_equal(diag(pvals), rep(1, 3), ignore_attr = TRUE)

	t_ab <- abs(10 - 11) / 0.5
	expected_ab <- 2 * stats::pt(-abs(t_ab), df = 10)
	expect_equal(pvals["A", "B"], expected_ab, tolerance = 1e-12)
})

test_that("get_diffs_from_pvalues flags significant pairs and adjusts", {
	pmat <- matrix(
		c(1, 0.001, 0.5, 0.001, 1, 0.5, 0.5, 0.5, 1),
		nrow = 3,
		dimnames = list(c("A", "B", "C"), c("A", "B", "C"))
	)

	# adjust = "none": diffs follow the raw matrix directly
	res <- biometryassist:::get_diffs_from_pvalues(
		pmat,
		sig = 0.05,
		adjust = "none"
	)
	expect_true(res$diffs[["B-A"]]) # 0.001 < 0.05
	expect_false(res$diffs[["C-A"]]) # 0.5 not < 0.05
	expect_false(res$diffs[["C-B"]])
	# adjusted matrix is symmetric with diagonal 1 and no NAs (all pairs tested)
	expect_equal(diag(res$adjusted_matrix), rep(1, 3), ignore_attr = TRUE)
	expect_false(anyNA(res$adjusted_matrix))

	# adjust = "bonferroni": p-values scaled by number of comparisons
	res_bonf <- biometryassist:::get_diffs_from_pvalues(
		pmat,
		sig = 0.05,
		adjust = "bonferroni"
	)
	expect_equal(
		unname(res_bonf$adjusted_pvalues),
		pmin(c(0.001, 0.5, 0.5) * 3, 1)
	)
})


# ============================================================================
# ORIGINAL TESTS (Updated to work with new structure)
# ============================================================================

test_that("mct produces output", {
	tmp <- withr::local_tempdir()
	withr::with_dir(tmp, {
		withr::local_file("Rplots.pdf")

		output <- multiple_comparisons(
			dat.aov,
			classify = "Species",
			plot = TRUE
		)
		while (grDevices::dev.cur() > 1) {
			grDevices::dev.off()
		}

		expect_equal(
			output$predictions$predicted.value,
			c(0.25, 1.33, 2.03),
			tolerance = 5e-2
		)
		ap <- autoplot(output)
		expect_autoplot_data(ap, output)
		expect_local_doppelganger("mct output", ap)
	})
})

test_that("mct ylab handles call/language labels", {
	pp <- data.frame(
		trt = factor(c("A", "B")),
		predicted.value = c(1, 2),
		std.error = c(0.1, 0.1),
		Df = c(10, 10),
		ci = c(0.2, 0.2),
		low = c(0.8, 1.8),
		up = c(1.2, 2.2),
		groups = c("a", "b")
	)

	# Note: This test uses internal function, may need adjustment
	# based on how add_attributes is updated
	out <- biometryassist:::add_attributes(
		pp,
		ylab = quote(sqrt(response)),
		crit_val = matrix(0, nrow = 2, ncol = 2),
		aliased_names = NULL
	)

	expect_identical(attr(out, "ylab"), quote(sqrt(response)))
})

test_that("add_attributes stores aliased and matrix HSD attributes", {
	pp <- data.frame(
		trt = factor(c("A", "B")),
		predicted.value = c(1, 2),
		std.error = c(0.1, 0.1),
		df = c(10, 10),
		ci = c(0.2, 0.2),
		low = c(0.8, 1.8),
		up = c(1.2, 2.2),
		groups = c("a", "b")
	)

	# Non-constant matrix ensures the var() check takes the matrix branch.
	crit_val <- matrix(c(0, 1, 2, 3), nrow = 2, ncol = 2)

	out <- biometryassist:::add_attributes(
		pp,
		ylab = quote(log(Petal.Width)),
		crit_val = crit_val,
		aliased_names = "B",
		trans = "log"
	)

	expect_identical(attr(out, "ylab"), "Petal.Width")
	expect_identical(attr(out, "aliased"), "B")
	expect_identical(attr(out, "HSD"), crit_val)
})

test_that("mct transformation: log", {
	dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
	output.log <- multiple_comparisons(
		dat.aov.log,
		classify = "Species",
		trans = "log",
		offset = 0
	)
	output.log2 <- multiple_comparisons(
		dat.aov.log,
		classify = "Species",
		trans = "log",
		offset = 0,
		int.type = "1se"
	)
	output.log3 <- multiple_comparisons(
		dat.aov.log,
		classify = "Species",
		trans = "log",
		offset = 0,
		int.type = "2se"
	)

	expect_identical(attr(output.log, "ylab"), "Petal.Width")
	expect_equal(
		output.log$predictions$predicted.value,
		c(-1.48, 0.27, 0.70),
		tolerance = 5e-2
	)
	expect_equal(
		output.log2$predictions$low,
		c(0.22, 1.26, 1.94),
		tolerance = 5e-2
	)
	expect_equal(
		output.log3$predictions$up,
		c(0.25, 1.41, 2.17),
		tolerance = 5e-2
	)
	ap <- autoplot(output.log)
	expect_autoplot_data(ap, output.log)
	expect_local_doppelganger("mct log output", ap)
})

test_that("mct transformation: sqrt", {
	dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
	output.sqrt <- multiple_comparisons(
		dat.aov.sqrt,
		classify = "Species",
		trans = "sqrt",
		offset = 0
	)
	output.sqrt2 <- multiple_comparisons(
		dat.aov.sqrt,
		classify = "Species",
		trans = "sqrt",
		offset = 0,
		int.type = "1se"
	)
	output.sqrt3 <- multiple_comparisons(
		dat.aov.sqrt,
		classify = "Species",
		trans = "sqrt",
		offset = 0,
		int.type = "2se"
	)

	expect_identical(attr(output.sqrt, "ylab"), "Petal.Width")
	expect_equal(
		output.sqrt$predictions$predicted.value,
		c(0.49, 1.15, 1.42),
		tolerance = 5e-2
	)
	expect_equal(
		output.sqrt2$predictions$low,
		c(0.23, 1.29, 1.98),
		tolerance = 5e-2
	)
	expect_equal(
		output.sqrt3$predictions$up,
		c(0.27, 1.38, 2.09),
		tolerance = 5e-2
	)
	ap <- autoplot(output.sqrt)
	expect_autoplot_data(ap, output.sqrt)
	expect_local_doppelganger("mct sqrt output", ap)
})

test_that("mct transformation: logit", {
	dat.aov.logit <- aov(logit(1 / Petal.Width) ~ Species, data = iris)
	output.logit <- multiple_comparisons(
		dat.aov.logit,
		classify = "Species",
		trans = "logit",
		offset = 0
	)
	output.logit2 <- multiple_comparisons(
		dat.aov.logit,
		classify = "Species",
		trans = "logit",
		offset = 0,
		int.type = "1se"
	)
	output.logit3 <- multiple_comparisons(
		dat.aov.logit,
		classify = "Species",
		trans = "logit",
		offset = 0,
		int.type = "2se"
	)

	expect_identical(attr(output.logit, "ylab"), "1/Petal.Width")
	expect_equal(
		output.logit$predictions$predicted.value,
		c(-5.30, -4.87, -3.07),
		tolerance = 5e-2
	)
	expect_equal(
		output.logit2$predictions$low,
		c(0.00, 0.01, 0.04),
		tolerance = 5e-2
	)
	expect_equal(
		output.logit3$predictions$up,
		c(0.01, 0.01, 0.05),
		tolerance = 5e-2
	)
	ap <- autoplot(output.logit)
	expect_autoplot_data(ap, output.logit)
	expect_local_doppelganger("mct logit output", ap)
})

test_that("mct transformation: inverse", {
	dat.aov.inverse <- aov((1 / Petal.Width) ~ Species, data = iris)
	output.inverse <- multiple_comparisons(
		dat.aov.inverse,
		classify = "Species",
		trans = "inverse",
		offset = 0
	)
	output.inverse2 <- multiple_comparisons(
		dat.aov.inverse,
		classify = "Species",
		trans = "inverse",
		offset = 0,
		int.type = "1se"
	)
	output.inverse3 <- multiple_comparisons(
		dat.aov.inverse,
		classify = "Species",
		trans = "inverse",
		offset = 0,
		int.type = "2se"
	)

	expect_identical(attr(output.inverse, "ylab"), "Petal.Width")
	expect_equal(
		output.inverse$predictions$predicted.value,
		c(0.50, 0.77, 4.79),
		tolerance = 5e-2
	)
	expect_equal(
		output.inverse2$predictions$up,
		c(3.01, 1.66, 0.22),
		tolerance = 5e-2
	)
	expect_equal(
		output.inverse3$predictions$low,
		c(1.20, 0.90, 0.20),
		tolerance = 5e-2
	)
	ap <- autoplot(output.inverse)
	expect_autoplot_data(ap, output.inverse)
	expect_local_doppelganger("mct inverse output", ap)
})

test_that("mct transformation: power", {
	iris_new <- iris
	iris_new$Petal.Width <- (iris_new$Petal.Width + 1)^3
	dat.aov.power <- aov(Petal.Width ~ Species, data = iris_new)
	output.power <- multiple_comparisons(
		dat.aov.power,
		classify = "Species",
		trans = "power",
		offset = 1,
		power = 3
	)
	output.power2 <- multiple_comparisons(
		dat.aov.power,
		classify = "Species",
		trans = "power",
		offset = 1,
		power = 3,
		int.type = "1se"
	)
	output.power3 <- multiple_comparisons(
		dat.aov.power,
		classify = "Species",
		trans = "power",
		offset = 1,
		power = 3,
		int.type = "2se"
	)

	expect_identical(attr(output.power, "ylab"), "Petal.Width")
	expect_equal(
		output.power$predictions$predicted.value,
		c(1.98, 12.85, 28.38),
		tolerance = 5e-2
	)
	expect_equal(
		output.power2$predictions$low,
		c(0.09, 1.30, 2.03),
		tolerance = 5e-2
	)
	expect_equal(
		output.power3$predictions$up,
		c(0.49, 1.42, 2.10),
		tolerance = 5e-2
	)
	ap <- autoplot(output.power)
	expect_autoplot_data(ap, output.power)
	expect_local_doppelganger("mct power output", ap)
})

test_that("mct transformation: arcsin", {
	iris_arc <- iris
	iris_arc$PW <- iris_arc$Petal.Width / max(iris_arc$Petal.Width)
	dat.aov.arcsin <- aov(asin(sqrt(PW)) ~ Species, data = iris_arc)
	expect_warning(
		output.arcsin <- multiple_comparisons(
			dat.aov.arcsin,
			classify = "Species",
			trans = "arcsin",
			offset = 0
		),
		"There are unevaluated constants in the response formula"
	)

	expect_identical(attr(output.arcsin, "ylab"), "PW")
	expect_true("PredictedValue" %in% names(output.arcsin$predictions))
	expect_true(all(
		output.arcsin$predictions$PredictedValue >= 0 &
			output.arcsin$predictions$PredictedValue <= 1
	))
	expected_pw_means <- tapply(iris_arc$PW, iris_arc$Species, mean)
	expect_equal(
		output.arcsin$predictions$PredictedValue,
		as.numeric(expected_pw_means[as.character(
			output.arcsin$predictions$Species
		)]),
		tolerance = 5e-2
	)
})

test_that("apply_transformation warns when offset is missing", {
	pp <- data.frame(
		predicted.value = c(0),
		std.error = c(0.1),
		ci = c(0.2)
	)

	expect_warning(
		out <- biometryassist:::apply_transformation(
			pp,
			trans = "log",
			offset = NULL,
			power = NULL
		),
		"Offset value assumed to be 0\\. Change with `offset` argument\\."
	)

	expect_true(all(
		c("PredictedValue", "ApproxSE", "low", "up") %in% names(out)
	))
})

test_that("multiple_comparisons warns when offset is omitted", {
	dat.aov <- aov(log(Petal.Width) ~ Species, data = iris)
	expect_warning(
		multiple_comparisons(dat.aov, classify = "Species", trans = "log"),
		"Offset value assumed to be 0\\. Change with `offset` argument\\."
	)
})

test_that("apply_transformation warns for invalid back-transformed values", {
	# sqrt: negative value after offset removal
	pp.sqrt <- data.frame(
		predicted.value = c(0.1),
		std.error = c(0.1),
		ci = c(0.2)
	)
	expect_warning(
		out.sqrt <- biometryassist:::apply_transformation(
			pp.sqrt,
			trans = "sqrt",
			offset = 1,
			power = NULL
		),
		"Square root back-transformation produced negative values\\. Check offset parameter\\."
	)
	expect_true(out.sqrt$PredictedValue < 0)

	# log: non-positive value after offset removal
	pp.log <- data.frame(
		predicted.value = c(0),
		std.error = c(0.1),
		ci = c(0.2)
	)
	expect_warning(
		out.log <- biometryassist:::apply_transformation(
			pp.log,
			trans = "log",
			offset = 1,
			power = NULL
		),
		"Log back-transformation produced non-positive values\\. Check offset parameter\\."
	)
	expect_true(out.log$PredictedValue <= 0)

	# logit: force exact boundary value with -Inf
	pp.logit <- data.frame(
		predicted.value = c(-Inf),
		std.error = c(0.1),
		ci = c(0.2)
	)
	expect_warning(
		out.logit <- biometryassist:::apply_transformation(
			pp.logit,
			trans = "logit",
			offset = 0,
			power = NULL
		),
		"Logit back-transformation produced values outside \\(0,1\\)"
	)
	expect_true(out.logit$PredictedValue <= 0 || out.logit$PredictedValue >= 1)
})

test_that("apply_transformation warns for inverse edge cases", {
	pp.inv <- data.frame(
		predicted.value = c(0.1),
		std.error = c(0.1),
		ci = c(0.2)
	)

	warnings <- character()
	out <- withCallingHandlers(
		biometryassist:::apply_transformation(
			pp.inv,
			trans = "inverse",
			offset = 0,
			power = NULL
		),
		warning = function(w) {
			warnings <<- c(warnings, conditionMessage(w))
			invokeRestart("muffleWarning")
		}
	)

	expect_true(any(grepl(
		"Inverse transformation: confidence interval crosses zero",
		warnings,
		fixed = TRUE
	)))
	expect_true("PredictedValue" %in% names(out))
	expect_true(out$low <= out$up)
})

test_that("apply_transformation warns for inverse near-zero predicted values", {
	pp.inv <- data.frame(
		predicted.value = c(1e-20),
		std.error = c(0.1),
		ci = c(1e-25)
	)

	expect_warning(
		out <- biometryassist:::apply_transformation(
			pp.inv,
			trans = "inverse",
			offset = 0,
			power = NULL
		),
		"Inverse transformation: predicted values very close to zero detected\\."
	)
	expect_true("PredictedValue" %in% names(out))
})

test_that("apply_transformation power validation and warnings", {
	pp <- data.frame(
		predicted.value = c(1),
		std.error = c(0.1),
		ci = c(0.2)
	)

	expect_error(
		biometryassist:::apply_transformation(
			pp,
			trans = "power",
			offset = 0,
			power = NULL
		),
		"Power transformation requires a non-zero numeric 'power' argument\\."
	)

	expect_warning(
		out <- biometryassist:::apply_transformation(
			pp,
			trans = "power",
			offset = 1,
			power = -1
		),
		"Power back-transformation with negative power produced values near zero\\."
	)
	expect_true("PredictedValue" %in% names(out))
})

test_that("apply_transformation arcsin warning and bounds", {
	pp <- data.frame(
		predicted.value = c(pi),
		std.error = c(0.1),
		ci = c(0.2)
	)

	expect_warning(
		out <- biometryassist:::apply_transformation(
			pp,
			trans = "arcsin",
			offset = 0,
			power = NULL
		),
		"Arcsin transformation: some predicted values outside \\[-pi/2, pi/2\\]\\."
	)

	expect_true("PredictedValue" %in% names(out))
	expect_true(out$PredictedValue >= 0 && out$PredictedValue <= 1)
})

test_that("apply_transformation arcsin warns when back-transformation is outside [0,1]", {
	# This branch is numerically very hard to reach because back_transform()'s
	# arcsin (sin(x)^2) is always in [0,1]. Here we temporarily override
	# `back_transform()` (which now computes PredictedValue) and `sqrt()` (used
	# for ApproxSE) in the apply_transformation() function environment to force an
	# out-of-bounds value and exercise the bounds-check warning.
	fn2 <- biometryassist:::apply_transformation
	fn2_env <- new.env(parent = environment(fn2))
	fn2_env$back_transform <- function(x, trans, offset = 0, power = NULL) {
		rep(2, length(x))
	}
	fn2_env$sqrt <- function(x) rep(0, length(x))
	environment(fn2) <- fn2_env

	pp <- data.frame(
		predicted.value = c(0),
		std.error = c(0.1),
		ci = c(0.2)
	)

	expect_warning(
		out <- fn2(pp, trans = "arcsin", offset = 0, power = NULL),
		"Arcsin back-transformation produced values outside \\[0,1\\]\\. This may indicate numerical issues\\."
	)

	expect_true("PredictedValue" %in% names(out))
	expect_true(out$PredictedValue > 1)
})

test_that("apply_transformation errors on invalid trans", {
	pp <- data.frame(
		predicted.value = c(0),
		std.error = c(0.1),
		ci = c(0.2)
	)

	expect_error(
		biometryassist:::apply_transformation(
			pp,
			trans = "not-a-transformation",
			offset = 0,
			power = NULL
		),
		"Invalid trans value\\. Must be one of: 'sqrt', 'log', 'logit', 'power', 'inverse', 'arcsin'\\.",
		fixed = FALSE
	)
})

test_that("ordering output works", {
	output1 <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		descending = FALSE
	)
	output2 <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		descending = TRUE
	)
	expect_equal(
		output1$predictions$predicted.value,
		c(0.25, 1.33, 2.03),
		tolerance = 5e-2
	)
	expect_equal(
		output2$predictions$predicted.value,
		c(2.03, 1.33, 0.25),
		tolerance = 5e-2
	)

	# Plot content (runs on every platform, incl. CI): the autoplot draws the
	# right means, interval bounds and letters. expect_autoplot_data() compares
	# as sets, so we additionally assert the ascending/descending ordering that
	# this test is specifically about.
	asc <- autoplot(output1)
	desc <- autoplot(output2)
	expect_autoplot_data(asc, output1)
	expect_autoplot_data(desc, output2)
	# Ordering: ascending rises left-to-right, descending falls.
	expect_false(is.unsorted(layer_data_for(asc, "GeomPoint")$y))
	expect_false(is.unsorted(rev(layer_data_for(desc, "GeomPoint")$y)))

	# Pixel-exact visual regression: local only (see expect_local_doppelganger).
	expect_local_doppelganger("mct ascending order", asc)
	expect_local_doppelganger("mct descending output", desc)
})

test_that("different interval types work", {
	output1 <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		int.type = "1se"
	)
	output2 <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		int.type = "2se"
	)
	expect_equal(output1$predictions$low, c(0.22, 1.30, 2.00), tolerance = 5e-2)
	expect_equal(output1$predictions$up, c(0.28, 1.36, 2.06), tolerance = 5e-2)
	expect_equal(output2$predictions$low, c(0.19, 1.27, 1.97), tolerance = 5e-2)
	expect_equal(output2$predictions$up, c(0.31, 1.39, 2.09), tolerance = 5e-2)

	ap1 <- autoplot(output1)
	ap2 <- autoplot(output2)
	expect_autoplot_data(ap1, output1)
	expect_autoplot_data(ap2, output2)
	expect_local_doppelganger("mct output 1se", ap1)
	expect_local_doppelganger("mct output 2se", ap2)
})

test_that("Testing asreml predictions", {
	skip_if_not_installed("Matrix")
	load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
	expect_warning(
		output <- multiple_comparisons(
			model.asr,
			classify = "Nitrogen",
			pred.obj = pred.asr,
			dendf = dendf
		),
		"Argument `pred\\.obj` has been deprecated and will be removed in a future version\\. Predictions are now performed internally in the function\\."
	)
	expect_equal(
		output$predictions$predicted.value,
		c(77.76, 100.15, 114.41, 123.23),
		tolerance = 5e-2
	)
	expect_snapshot_output(output)
	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("asreml predictions", ap)
})

test_that("save produces output", {
	withr::local_file("pred_vals.csv")
	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		save = TRUE,
		savename = "pred_vals"
	)
	expect_snapshot_output(output)

	# CSV contains full precision values
	expect_csv_matches_df(output$predictions, "pred_vals.csv")
})

test_that("Interaction terms work", {
	load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
	skip_if_not_installed("asreml")
	quiet(library(asreml))
	output <- multiple_comparisons(
		model.asr,
		classify = "Nitrogen:Variety",
		pvals = T
	)
	expect_equal(
		output$predictions$predicted.value,
		c(
			70.85,
			76.58,
			85.86,
			92.22,
			99.91,
			108.32,
			113.1,
			113.5,
			116.63,
			118.4,
			123.75,
			127.53
		),
		tolerance = 5e-2
	)

	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("Interactions work", ap)
})

test_that("order argument is deprecated", {
	# dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_warning(
		multiple_comparisons(dat.aov, classify = "Species", order = "xyz"),
		"Argument `order` has been deprecated and will be removed in a future version. Please use `descending` instead."
	)
})

test_that("dashes are handled", {
	iris2 <- iris
	# Replace 'gin' in setosa with '-'
	iris2$Species <- as.factor(gsub("to", "-", iris2$Species))
	dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)
	output2 <- suppressWarnings(multiple_comparisons(
		dat.aov2,
		classify = "Species"
	))
	expect_warning(
		multiple_comparisons(dat.aov2, classify = "Species"),
		"The treatment level se-sa contained '-', which has been replaced in the final output with '_'"
	)
	expect_equal(
		output2$predictions$predicted.value,
		c(0.25, 1.33, 2.03),
		tolerance = 5e-2
	)

	# Replace 'gin' in virginica with '-' as well
	iris2$Species <- as.factor(gsub("gin", "-", iris2$Species))
	dat.aov2 <- aov(Petal.Width ~ Species, data = iris2)

	expect_warning(
		multiple_comparisons(dat.aov2, classify = "Species"),
		"The treatment levels se-sa, vir-ica contained '-', which has been replaced in the final output with '_'"
	)
	expect_equal(
		output2$predictions$predicted.value,
		c(0.25, 1.33, 2.03),
		tolerance = 5e-2
	)

	# skip_if(interactive())
	ap <- autoplot(output2)
	expect_autoplot_data(ap, output2)
	expect_local_doppelganger("mct dashes output", ap)
})

test_that("mct removes aliased treatments in aov", {
	CO_2 <- CO2
	CO_2$uptake[CO_2$Type == "Quebec" & CO_2$Treatment == "nonchilled"] <- NA
	model <- aov(uptake ~ Type + Treatment + Type:Treatment, data = CO_2)
	expect_warning(
		output1 <- multiple_comparisons(model, classify = "Type:Treatment"),
		"A level of Type\\:Treatment is aliased\\. It has been removed from predicted output\\.\n  Aliased level is\\: Quebec:nonchilled\\.\n  This level is saved as an attribute of the output object\\."
	)
	expect_snapshot_output(output1$predictions$predicted.value)
	# skip_if(interactive())
	ap <- autoplot(output1)
	expect_autoplot_data(ap, output1)
	expect_local_doppelganger("aov aliased output", ap)
})


test_that("mct handles aliased results in asreml with a warning", {
	skip_if_not_installed("asreml")
	quiet(library(asreml))
	load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
	load(test_path("data", "oats_data_missing.Rdata"), envir = .GlobalEnv)
	expect_warning(
		output <- multiple_comparisons(
			model.asr,
			classify = "Nitrogen:Variety"
		),
		"Aliased level is: 0\\.2_cwt:Golden_rain\\."
	)
	expect_snapshot_output(output)

	load(test_path("data", "oats_data_missing2.Rdata"), envir = .GlobalEnv)

	expect_warning(
		output <- multiple_comparisons(
			model2.asr,
			classify = "Nitrogen:Variety"
		),
		"Some levels of Nitrogen:Variety are aliased\\. They have been removed from predicted output\\."
	)
	expect_snapshot_output(output)
	expect_warning(
		multiple_comparisons(model2.asr, classify = "Nitrogen:Variety"),
		"Aliased levels are: 0\\.2_cwt:Golden_rain, 0\\.2_cwt:Victory\\."
	)
})

test_that("Invalid classify argument causes an error", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_error(
		multiple_comparisons(dat.aov, classify = "ABC"),
		"ABC is not a term in the model\\. Please check model specification\\."
	)
	expect_error(
		multiple_comparisons(model.asr, classify = "ABC"),
		"ABC is not a term in the model\\. Please check model specification\\."
	)
})

test_that("Significance values that are too high give a warning or error", {
	# dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_warning(
		multiple_comparisons(dat.aov, classify = "Species", sig = 0.95),
		"Significance level given by `sig` is high. Perhaps you meant 0.05?"
	)
	expect_error(
		multiple_comparisons(dat.aov, classify = "Species", sig = 5),
		"Significance level given by `sig` is high. Perhaps you meant 0.05?"
	)
	expect_error(
		multiple_comparisons(dat.aov, classify = "Species", sig = 95),
		"Significance level given by `sig` is high. Perhaps you meant 0.05?"
	)
})

test_that("Use of pred argument gives warning", {
	# dat.aov <- aov(Petal.Width ~ Species, data = iris)
	expect_warning(
		multiple_comparisons(dat.aov, classify = "Species", pred = "Species"),
		"Argument `pred` has been deprecated and will be removed in a future version. Please use `classify` instead."
	)
})

test_that("Invalid column name causes an error", {
	dat <- design(
		"crd",
		LETTERS[1:4],
		4,
		nrow = 4,
		ncols = 4,
		quiet = TRUE
	)$design
	names(dat)[5] <- "groups"
	dat.aov <- aov(rnorm(16, 10) ~ groups, data = dat)

	expect_error(
		multiple_comparisons(dat.aov, classify = "groups"),
		"Invalid column name. Please change the name of column\\(s\\): groups"
	)
})

test_that("Including pred.obj object causes warning", {
	skip_if_not_installed("asreml")
	quiet(library(asreml))
	load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
	expect_warning(
		multiple_comparisons(
			model.asr,
			pred.obj = pred.asr,
			classify = "Nitrogen"
		),
		"Argument \\`pred.obj\\` has been deprecated and will be removed in a future version\\. Predictions are now performed internally in the function\\."
	)
})

test_that("Providing a random term in classify produces an error.", {
	skip_if_not_installed("asreml")
	load(test_path("data", "oats_data_missing2.Rdata"), envir = .GlobalEnv)
	expect_error(
		multiple_comparisons(model2.asr, classify = "Blocks"),
		"All predicted values are aliased\\. Perhaps you need the `present` argument\\?"
	)
})

test_that("lme4 model works", {
	skip_if_not_installed("lme4")
	quiet(library(lme4))
	dat.lmer <- lmer(yield ~ Nitrogen * Variety + (1 | Blocks), data = dat)
	output <- multiple_comparisons(dat.lmer, classify = "Nitrogen")
	expect_equal(output$predictions$std.error, rep(7.39, 4), tolerance = 5e-2)
	expect_equal(
		min(output$predictions$predicted.value),
		79.39,
		tolerance = 5e-2
	)
	expect_equal(
		max(output$predictions$predicted.value),
		123.39,
		tolerance = 5e-2
	)
	expect_equal(
		output$predictions$predicted.value,
		c(79.39, 98.89, 114.22, 123.39),
		tolerance = 5e-2
	)
	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("lme4 output", ap)
})

test_that("3 way interaction works", {
	des <- design(
		type = "crossed:crd",
		treatments = c(3, 3, 3),
		reps = 3,
		nrows = 9,
		ncols = 9,
		seed = 42,
		quiet = TRUE
	)
	des$design$response <- rnorm(81, 100)
	des$design$A <- factor(des$design$A)
	des$design$B <- factor(des$design$B)
	des$design$C <- factor(des$design$C)
	dat.aov <- aov(response ~ A * B * C, data = des$design)
	output <- multiple_comparisons(dat.aov, classify = "A:B:C")
	expect_snapshot_output(output$predictions$predicted.value)
	expect_equal(output$predictions$std.error, rep(0.63, 27), tolerance = 5e-2)
	# skip_if(interactive())
	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger(
		"3 way interaction",
		ap,
		variant = ggplot2_variant()
	)
})

test_that("plots are produced when requested", {
	withr::local_seed(123)
	des <- design(
		type = "crossed:crd",
		treatments = c(3, 3, 3),
		reps = 3,
		nrows = 9,
		ncols = 9,
		seed = 42,
		quiet = TRUE
	)
	des$design$response <- rnorm(81, 100)
	des$design$A <- factor(des$design$A)
	des$design$B <- factor(des$design$B)
	des$design$C <- factor(des$design$C)
	dat.aov <- aov(response ~ A * B * C, data = des$design)

	tmp <- withr::local_tempdir()
	withr::with_dir(tmp, {
		withr::local_file("Rplots.pdf")

		expect_snapshot_output(
			output <- multiple_comparisons(
				dat.aov,
				classify = "A:B:C",
				plot = TRUE
			)
		)
		while (grDevices::dev.cur() > 1) {
			grDevices::dev.off()
		}

		expect_s3_class(output, "mct")
		expect_equal(nrow(output$predictions), 27)
		expect_equal(output$predictions$std.error, rep(0.63, 27), tolerance = 5e-2)

		skip_if(interactive())
		expect_local_doppelganger(
			"3 way interaction internal",
			function() {
				invisible(multiple_comparisons(
					dat.aov,
					classify = "A:B:C",
					plot = TRUE
				))
			},
			variant = ggplot2_variant()
		)
		while (grDevices::dev.cur() > 1) {
			grDevices::dev.off()
		}
	})
})

test_that("nlme/lme model is supported", {
	skip_if_not_installed("nlme")
	suppressPackageStartupMessages(library(nlme))
	load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
	dat.lme <- lme(
		yield ~ Nitrogen * Variety,
		random = ~ 1 | Blocks,
		data = dat
	)
	output <- multiple_comparisons(dat.lme, classify = "Nitrogen")
	expect_equal(output$predictions$std.error, rep(7.39, 4), tolerance = 5e-2)
	expect_equal(
		min(output$predictions$predicted.value),
		79.39,
		tolerance = 5e-2
	)
	expect_equal(
		max(output$predictions$predicted.value),
		123.39,
		tolerance = 5e-2
	)
	expect_equal(
		output$predictions$predicted.value,
		c(79.39, 98.89, 114.22, 123.39),
		tolerance = 5e-2
	)
	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("nlme output", ap)
})

test_that("afex (afex_aov) model is supported", {
	skip_if_not_installed("afex")
	data(obk.long, package = "afex")

	# Between-subjects factorial design.
	afex_b <- afex::aov_ez(
		id = "id",
		dv = "value",
		between = c("treatment", "gender"),
		data = obk.long,
		fun_aggregate = mean
	)
	output <- multiple_comparisons(afex_b, classify = "treatment")

	expect_s3_class(output, "mct")
	expect_equal(
		output$predictions$predicted.value,
		c(4.22, 6.03, 6.25),
		tolerance = 5e-2
	)
	# No significant treatment differences here, so all share a single letter group.
	expect_equal(unique(output$predictions$groups), "a")

	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("afex output", ap)
})

test_that("glmmTMB model is supported", {
	skip_if_not_installed("glmmTMB")
	data(Salamanders, package = "glmmTMB")

	g <- glmmTMB::glmmTMB(
		count ~ spp + mined + (1 | site),
		data = Salamanders,
		family = gaussian()
	)
	output <- multiple_comparisons(g, classify = "mined")

	expect_s3_class(output, "mct")
	expect_equal(
		output$predictions$predicted.value,
		c(0.30, 2.26),
		tolerance = 5e-2
	)
	expect_equal(output$predictions$groups, c("a", "b"))

	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("glmmTMB output", ap)
})

test_that("sommer mmes model is supported", {
	skip_if_not_installed("sommer")
	load(test_path("data", "sommer_models.Rdata"), .GlobalEnv)

	output <- multiple_comparisons(model_mmes, classify = "Env")

	expect_s3_class(output, "mct")
	# Env effect: CA.2011 separates from the CA.2012/CA.2013 group.
	expect_setequal(
		round(output$predictions$predicted.value, 2),
		c(10.12, 10.72, 16.50)
	)

	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("sommer mmes output", ap)
})

test_that("lme4breeding (lmebreed) model is supported", {
	skip_if_not_installed("lme4breeding")
	# lmebreed() relies on lme4 internals being attached (lme4 is in its Depends).
	suppressPackageStartupMessages(library(lme4breeding))
	load(test_path("data", "oats_data.Rdata"), .GlobalEnv)

	# lmebreed() objects carry class `lmerMod` and use the existing lmerMod method.
	# An identity relationship matrix gives an exact lme4::lmer() reference.
	blocks <- levels(factor(dat$Blocks))
	A <- diag(length(blocks))
	dimnames(A) <- list(blocks, blocks)

	m_lmb <- suppressMessages(suppressWarnings(lmebreed(
		yield ~ Nitrogen + (1 | Blocks),
		relmat = list(Blocks = A),
		data = dat,
		verbose = FALSE,
		dateWarning = FALSE
	)))
	output <- suppressMessages(multiple_comparisons(m_lmb, classify = "Nitrogen"))

	expect_s3_class(output, "mct")
	expect_equal(nrow(output$predictions), 4)

	out_lmer <- multiple_comparisons(
		lme4::lmer(yield ~ Nitrogen + (1 | Blocks), data = dat),
		classify = "Nitrogen"
	)
	expect_equal(
		output$predictions$predicted.value,
		out_lmer$predictions$predicted.value,
		tolerance = 1e-4
	)
})

test_that("invalid model types give a clear error", {
	# Use an unsupported model type that still has a `formula()` method so that
	# the error comes from `get_predictions.default()` (not from validate_inputs()).
	dat <- data.frame(x = 1:10, y = 1:10)
	m <- stats::nls(
		density ~ SSlogis(log(conc), Asym, xmid, scal),
		data = subset(DNase, Run == 1)
	)

	expect_error(
		multiple_comparisons(m, classify = "x"),
		"model\\.obj must be a linear \\(mixed\\) model object\\.",
		fixed = FALSE
	)
})

test_that("multiple_comparisons output has a class of 'mct'", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	expect_s3_class(output, "mct")
})

test_that("Setting groups to FALSE disables letter groups", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	expect_true("groups" %in% colnames(output$predictions))
	expect_equal(output$predictions$groups, c("a", "b", "c"))

	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		groups = FALSE
	)
	expect_false("groups" %in% colnames(output$predictions))

	output <- multiple_comparisons(
		dat.aov,
		classify = "Species",
		letters = FALSE
	)
	expect_false("groups" %in% colnames(output$predictions))

	ap <- autoplot(output)
	expect_autoplot_data(ap, output)
	expect_local_doppelganger("No letter groups", ap)
})

test_that("Check for letters as an alias of groups", {
	expect_warning(
		output <- multiple_comparisons(
			dat.aov,
			classify = "Species",
			groups = FALSE,
			letters = TRUE
		),
		"Both 'groups' and 'letters' provided\\. Using 'groups'\\."
	)
	expect_false("groups" %in% colnames(output$predictions))
})

test_that("autoplot supports legacy mct data.frame objects", {
	# Old versions of biometryassist used an `mct` object that was itself a data.frame.
	# This test covers the backward-compatibility branch in autoplot.mct().
	legacy <- data.frame(
		Species = factor(c("setosa", "versicolor", "virginica")),
		predicted.value = c(0.25, 1.33, 2.03),
		std.error = c(0.02, 0.02, 0.02),
		ci = c(0.05, 0.05, 0.05),
		low = c(0.20, 1.28, 1.98),
		up = c(0.30, 1.38, 2.08),
		groups = c("a", "b", "c")
	)
	class(legacy) <- c("mct", class(legacy))
	attr(legacy, "ylab") <- "Petal.Width"

	p <- autoplot(legacy)
	expect_s3_class(p, "ggplot")
})

test_that("autoplot can rotate axis and labels independently", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	# The plotted data is identical regardless of rotation; check it once.
	expect_autoplot_data(autoplot(output), output)
	expect_local_doppelganger(
		"label rotation",
		autoplot(output, label_rotation = 90)
	)
	expect_local_doppelganger(
		"axis rotation",
		autoplot(output, axis_rotation = 90)
	)
	expect_local_doppelganger(
		"axis rotation -90",
		autoplot(output, axis_rotation = -90)
	)
	expect_local_doppelganger(
		"axis and label rotation",
		autoplot(output, axis_rotation = 45, label_rotation = 90)
	)
	expect_local_doppelganger(
		"rotation and axis rotation",
		autoplot(output, rotation = 45, axis_rotation = 90)
	)
	expect_local_doppelganger(
		"rotation and label rotation",
		autoplot(output, rotation = 45, label_rotation = 90)
	)
})

test_that("Autoplot can output column graphs", {
	output <- multiple_comparisons(dat.aov, classify = "Species")
	p1 <- autoplot(output, type = "column", label_height = 1)
	p2 <- autoplot(output, type = "col", label_height = 1)
	expect_in("GeomCol", class(p1$layers[[1]]$geom))
	expect_in("GeomCol", class(p2$layers[[1]]$geom))
	expect_true(equivalent_ggplot2(p1, p2))
	expect_local_doppelganger("autoplot column", p1)
})

test_that("A warning is printed if a transformation is detected with no trans argument provided", {
	dat.aov.log <- aov(log(Petal.Width) ~ Species, data = iris)
	dat.aov.sqrt <- aov(sqrt(Petal.Width) ~ Species, data = iris)
	dat.aov.logit <- aov(logit(1 / Petal.Width) ~ Species, data = iris)
	dat.aov.inverse <- aov((1 / Petal.Width) ~ Species, data = iris)
	dat.aov.power <- aov(Petal.Width^3 ~ Species, data = iris)

	expect_warning(
		multiple_comparisons(dat.aov.log, classify = "Species"),
		"The response variable appears to be transformed in the model formula: log\\(Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\."
	)
	expect_warning(
		multiple_comparisons(dat.aov.sqrt, classify = "Species"),
		"The response variable appears to be transformed in the model formula: sqrt\\(Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\."
	)
	expect_warning(
		multiple_comparisons(dat.aov.logit, classify = "Species"),
		"The response variable appears to be transformed in the model formula: logit\\(1/Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\."
	)
	expect_warning(
		multiple_comparisons(dat.aov.inverse, classify = "Species"),
		"The response variable appears to be transformed in the model formula: \\(1/Petal\\.Width\\)\\.
Please specify the 'trans' argument if you want back-transformed predictions\\."
	)
	expect_warning(
		multiple_comparisons(dat.aov.power, classify = "Species"),
		"The response variable appears to be transformed in the model formula: Petal\\.Width\\^3\\.
Please specify the 'trans' argument if you want back-transformed predictions\\."
	)
})


# Test aliased output prints
test_that("print.mct with no aliased attribute", {
	dat.aov <- aov(Petal.Width ~ Species, data = iris)
	output <- multiple_comparisons(dat.aov, classify = "Species", plot = FALSE)

	# Manually set aliased for testing (in new structure)
	output$aliased <- "ABC"
	attr(output, "aliased") <- "ABC"

	expect_true("aliased" %in% names(output))
	expect_length(output$aliased, 1)
	expect_output(print(output), "Aliased level is: ABC")

	output$aliased <- c("ABC", "DEF")
	attr(output, "aliased") <- c("ABC", "DEF")

	expect_length(output$aliased, 2)
	expect_true("aliased" %in% names(output))
	expect_output(print(output), "Aliased levels are: ABC and DEF")

	output$aliased <- c("ABC", "DEF", "GHI")
	attr(output, "aliased") <- c("ABC", "DEF", "GHI")

	expect_length(output$aliased, 3)
	expect_true("aliased" %in% names(output))
	expect_output(print(output), "Aliased levels are: ABC, DEF and GHI")
})

test_that("Full precision values are stored in predictions", {
	# Create data with very small standard errors
	set.seed(123)

	# Create a mock model that would produce very small standard errors
	# We'll use a simple approach with very precise data
	precise_data <- data.frame(
		treatment = factor(rep(c("A", "B", "C"), each = 100)),
		response = c(
			rep(1.000001, 100),
			rep(1.000002, 100),
			rep(1.000003, 100)
		) +
			rnorm(300, 0, 0.0000001) # Very small error
	)

	dat.aov <- aov(response ~ treatment, data = precise_data)

	# Values are stored at full precision
	output <- multiple_comparisons(
		dat.aov,
		classify = "treatment"
	)

	# Check that standard errors are preserved at full precision (not rounded to 0)
	expect_true(all(output$predictions$std.error > 0))
	expect_false(any(output$predictions$std.error == 0))

	# Full precision predicted values should have more than 2 decimal places
	expect_true(any(
		nchar(sub(
			".*\\.",
			"",
			as.character(output$predictions$predicted.value)
		)) >
			2
	))
})

test_that("Full precision values stored with transformed data", {
	# Create data that will have small standard errors on the transformed scale
	set.seed(456)
	# Use data that's already on log scale with very small differences
	transform_data <- data.frame(
		treatment = factor(rep(c("A", "B", "C"), each = 100)),
		# Create log-scale response with tiny differences and very small error
		log_response = c(
			rep(-0.0001, 100),
			rep(0.0000, 100),
			rep(0.0001, 100)
		) +
			rnorm(300, 0, 0.000001) # Very small error on log scale
	)

	# Apply log transformation - the model is on log scale, transform back to original
	dat.aov <- aov(log_response ~ treatment, data = transform_data)

	# No rounding warning - full precision stored
	output <- multiple_comparisons(
		dat.aov,
		classify = "treatment",
		trans = "log",
		offset = 0
	)

	# Check that both standard error columns are preserved at full precision
	expect_true(all(output$predictions$std.error > 0))
	expect_true(all(output$predictions$ApproxSE > 0))
	expect_false(any(output$predictions$std.error == 0))
	expect_false(any(output$predictions$ApproxSE == 0))
})

test_that("Decimals argument in print.mct controls rounding", {
	# Use the standard iris data which has reasonable standard errors
	dat.aov <- aov(Petal.Width ~ Species, data = iris)

	output <- multiple_comparisons(
		dat.aov,
		classify = "Species"
	)

	# Full precision values are stored
	expect_true(is.numeric(output$predictions$predicted.value))
	expect_true(is.numeric(output$predictions$std.error))

	# Print output with decimals = 2 should show rounded values
	printed <- capture.output(print(output, decimals = 2))
	expect_true(any(grepl("0\\.25|0\\.246", printed)))

	# Print output with decimals = 4 should show more precision
	printed4 <- capture.output(print(output, decimals = 4))
	expect_true(any(grepl("0\\.246", printed4)))
})

test_that("Decimals parameter in multiple_comparisons is deprecated", {
	# Create data with moderately small standard errors
	set.seed(789)
	moderate_data <- data.frame(
		treatment = factor(rep(c("A", "B"), each = 20)),
		response = c(rep(1.001, 20), rep(1.002, 20)) + rnorm(40, 0, 0.001)
	)

	dat.aov <- aov(response ~ treatment, data = moderate_data)

	# Using decimals in multiple_comparisons should produce a deprecation warning
	expect_warning(
		multiple_comparisons(dat.aov, classify = "treatment", decimals = 4),
		"decimals.*deprecated"
	)

	# Full precision standard errors are always stored
	output <- multiple_comparisons(dat.aov, classify = "treatment")
	expect_true(all(output$predictions$std.error > 0))
})


test_that("ApproxSE column preserves full precision", {
	# Create data that will trigger the standard error preservation
	set.seed(111)
	# Create data with extremely small values to get tiny standard errors after log transformation
	precise_data <- data.frame(
		treatment = factor(rep(c("A", "B"), each = 100)),
		# Use very small response values that will have tiny standard errors
		response = c(rep(0.000001, 100), rep(0.000002, 100)) +
			rnorm(200, 0, 0.0000001)
	)

	# Use log transformation to create ApproxSE column
	dat.aov <- aov(log(response) ~ treatment, data = precise_data)

	# No rounding warning - full precision stored
	output <- multiple_comparisons(
		dat.aov,
		classify = "treatment",
		trans = "log",
		offset = 0
	)

	# Both standard error columns should be preserved at full precision
	expect_true(all(output$predictions$std.error > 0))
	expect_true(all(output$predictions$ApproxSE > 0))
	expect_false(any(output$predictions$std.error == 0))
	expect_false(any(output$predictions$ApproxSE == 0))
})

test_that("Multiple comparisons works with aovlist objects", {
	# load in oats data
	load(test_path("data", "oats_data.Rdata"), .GlobalEnv)
	oats.aovlist <- aov(
		yield ~ Variety * Nitrogen + Error(Blocks / Wplots),
		data = dat
	)
	pred.aovlist <- multiple_comparisons(
		model.obj = oats.aovlist,
		classify = "Nitrogen"
	)

	# check HSD value
	expect_equal(pred.aovlist$hsd, 11.833, tolerance = 5e-2)
	expect_equal(pred.aovlist$pairwise_pvalues[3, 4], 0.180, tolerance = 5e-2)
})

test_that("Multiple comparisons for asreml objects provides the same results as an aovlist object for oats data", {
	# load in oats data
	skip_if_not_installed("asreml")
	load(test_path("data", "oats_data.Rdata"), .GlobalEnv)
	library(asreml)
	oats.asr <- asreml(
		yield ~ Variety * Nitrogen,
		random = ~ Blocks / Wplots,
		residual = ~units,
		data = dat
	)
	pred.asr <- multiple_comparisons(
		model.obj = oats.asr,
		classify = "Nitrogen"
	)

	# check HSD value
	expect_equal(pred.asr$hsd, 11.833, tolerance = 5e-2)
	# Check p-values matrix
	expect_equal(pred.asr$pairwise_pvalues[3, 4], 0.180, tolerance = 5e-2)
})

test_that("Multiple comparisons for lmer objects provides the same results as an aovlist object", {
	skip_if_not_installed("lme4")
	# load in oats data
	load(test_path("data", "oats_data.Rdata"), .GlobalEnv)
	library(lme4)
	oats.lme <- lme4::lmer(
		yield ~ Variety * Nitrogen + (1 | Blocks / Wplots),
		data = dat
	)
	pred.lme <- multiple_comparisons(
		model.obj = oats.lme,
		classify = "Nitrogen"
	)

	# check HSD value
	expect_equal(pred.lme$hsd, 11.833, tolerance = 5e-2)
	# check p-values matrix
	expect_equal(pred.lme$pairwise_pvalues[3, 4], 0.180, tolerance = 5e-2)
})

test_that("Multiple comparisons for lmerTest objects provides the same results as an aovlist object", {
	skip_if_not_installed("lmerTest")
	# load in oats data
	load(test_path("data", "oats_data.Rdata"), .GlobalEnv)
	library(lme4)
	library(lmerTest)
	oats.lmet <- lmerTest::lmer(
		yield ~ Variety * Nitrogen + (1 | Blocks / Wplots),
		data = dat
	)
	pred.lmet <- multiple_comparisons(
		model.obj = oats.lmet,
		classify = "Nitrogen"
	)

	# check HSD value
	expect_equal(pred.lmet$hsd, 11.833, tolerance = 5e-2)
	# check p-values matrix
	expect_equal(pred.lmet$pairwise_pvalues[3, 4], 0.180, tolerance = 5e-2)
})

test_that("get_predictions() returns exact SED for unbalanced marginal means", {
	skip_if_not_installed("emmeans")
	# Unbalanced two-factor design: predicting the `tension` margin averages over
	# an unbalanced `wool`, where the old sigma * sqrt(1/w_i + 1/w_j) form was
	# wrong. The SED must equal emmeans' exact pairwise SE (sqrt(V_ii+V_jj-2V_ij)).
	set.seed(1)
	wb <- warpbreaks[-sample(which(warpbreaks$wool == "A"), 10), ]
	m <- aov(breaks ~ wool * tension, data = wb)

	res <- get_predictions(m, "tension")
	sed <- sort(res$sed[upper.tri(res$sed)])

	emm <- emmeans::emmeans(m, ~tension)
	exact <- sort(
		as.data.frame(
			emmeans::contrast(emm, method = "pairwise")
		)$SE
	)

	expect_equal(sed, exact, tolerance = 1e-8)
})
