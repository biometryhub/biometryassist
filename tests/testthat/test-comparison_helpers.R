# Tests for internal helpers in R/comparison_helpers.R that are not exercised
# through the public API in the other test files.

test_that("validate_inputs: sig in [1, 50) stops suggesting sig/100", {
	m <- aov(Petal.Width ~ Species, data = iris)
	expect_error(
		pairwise_comparisons(m, classify = "Species", sig = 5),
		"Perhaps you meant 0\\.05"
	)
})

test_that("validate_inputs: sig >= 50 stops suggesting 1 - sig/100", {
	m <- aov(Petal.Width ~ Species, data = iris)
	expect_error(
		pairwise_comparisons(m, classify = "Species", sig = 95),
		"Perhaps you meant 0\\.05"
	)
})

test_that("validate_inputs: sig in [0.5, 1) warns suggesting 1 - sig", {
	m <- aov(Petal.Width ~ Species, data = iris)
	expect_warning(
		pairwise_comparisons(m, classify = "Species", sig = 0.6),
		"Perhaps you meant 0\\.4"
	)
})

test_that("check_reserved_names: applies to classify as resolved by the engine", {
	# A fake engine that resolves a wrapped classify as get_predictions.asreml()
	# does: "at(groups):Trt" becomes "groups:Trt". The reserved name is hidden
	# inside the wrapper in the input, so it must be caught after resolution.
	registerS3method(
		"get_predictions",
		"ch_fake_wrapped_classify",
		function(model.obj, classify, ...) {
			list(
				predictions = data.frame(
					groups = factor(c("a", "b")),
					Trt = factor(c("x", "x")),
					predicted.value = c(1, 2),
					std.error = c(0.5, 0.5)
				),
				sed = matrix(c(NA, 0.7, 0.7, NA), nrow = 2),
				vcov = diag(2) * 0.25,
				df = 10,
				ylab = "response",
				aliased_names = NULL,
				emmeans_grid = NULL,
				classify = "groups:Trt"
			)
		},
		envir = asNamespace("biometryassist")
	)
	m <- structure(list(), class = "ch_fake_wrapped_classify")
	msg <- "Invalid column name\\. Please change the name of column\\(s\\): groups"

	expect_error(multiple_comparisons(m, classify = "at(groups):Trt"), msg)
	expect_error(pairwise_comparisons(m, classify = "at(groups):Trt"), msg)
	expect_error(
		reference_comparisons(m, classify = "at(groups):Trt", reference = "a"),
		msg
	)
})

test_that("aliased_note: more than 6 aliased levels is condensed to a count", {
	result <- biometryassist:::aliased_note(letters[1:7])
	expect_match(result, "^7 levels are aliased")
	expect_match(result, "attr\\(x")
})

test_that("note_ci_padjust_mismatch: returns FALSE when required columns are absent", {
	x <- data.frame(comparison = "A - B", p.value = 0.01)
	expect_false(biometryassist:::note_ci_padjust_mismatch(x, 0.05, "holm"))
})

test_that("note_ci_padjust_mismatch: emits a message and returns TRUE when CI and adjusted p disagree", {
	# CI excludes zero (conf.low > 0) but adjusted p-value is not significant
	x <- data.frame(conf.low = 0.1, conf.high = 0.5, p.value = 0.2)
	expect_message(
		result <- biometryassist:::note_ci_padjust_mismatch(x, 0.05, "holm"),
		"confidence intervals are per-comparison"
	)
	expect_true(result)
})

test_that("dunnett_adjust errors when mvtnorm is not available", {
	skip_if_not_installed("mockery")
	fn <- biometryassist:::dunnett_adjust
	mockery::stub(fn, "requireNamespace", function(pkg, quietly = FALSE) FALSE)
	expect_error(
		fn(
			i_idx = 1L,
			j_idx = 2L,
			se = 1.0,
			df_ij = 10L,
			tstat = 2.0,
			vcov = matrix(c(1, 0, 0, 1), 2, 2),
			sig = 0.05
		),
		"Package 'mvtnorm' is required"
	)
})
