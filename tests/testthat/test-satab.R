test_that("print.satab prints to console", {
	obj <- satab(outdesign_crd_satab)
	expect_s3_class(obj, "satab")

	expect_invisible(print(obj))
	expect_output(print(obj), "Source of Variation")
	expect_output(print(obj), "Residual")
})

test_that("satab examples print expected sections", {
	expect_output(print(satab(outdesign_crd_satab)), "Source of Variation")
	expect_output(print(satab(outdesign_rcbd_satab)), "Block stratum")
	expect_output(print(satab(outdesign_lsd_satab)), "Row")

	expect_output(print(satab(outdesign_crossed_satab)), "Residual")
	expect_output(print(satab(outdesign_crossed_rcbd_satab)), "Residual")
	expect_output(print(satab(outdesign_crossed_lsd_satab)), "Residual")

	expect_output(print(satab(outdesign_nested_satab)), "Column")
	expect_output(print(satab(outdesign_split_satab)), "Whole plot Residual")
})

test_that("3-way factorial designs include 2-way and 3-way interactions", {
	fac3 <- agricolae::design.ab(
		trt = c(2, 3, 4),
		r = 2,
		design = "crd",
		seed = 42
	)
	res <- biometryassist:::anova_factorial_crd(fac3$book)

	# Factor names are whatever agricolae outputs (typically A, B, C)
	structural_cols <- c(
		"plots",
		"plot",
		"r",
		"rep",
		"reps",
		"block",
		"row",
		"col",
		"wholeplots",
		"wplots",
		"subplots",
		"splots",
		"treatments"
	)
	trt_names <- names(fac3$book)[!names(fac3$book) %in% structural_cols]
	expect_equal(length(trt_names), 3)

	A <- trt_names[1]
	B <- trt_names[2]
	C <- trt_names[3]

	# Main effect dfs
	expected_main <- c((2 - 1), (3 - 1), (4 - 1))
	names(expected_main) <- trt_names

	# Interaction dfs
	expected_interactions <- c(
		`A:B` = expected_main[A] * expected_main[B],
		`A:C` = expected_main[A] * expected_main[C],
		`B:C` = expected_main[B] * expected_main[C],
		`A:B:C` = expected_main[A] * expected_main[B] * expected_main[C]
	)
	names(expected_interactions) <- c(
		paste(A, B, sep = ":"),
		paste(A, C, sep = ":"),
		paste(B, C, sep = ":"),
		paste(A, B, C, sep = ":")
	)

	df_map <- stats::setNames(res$df, res$sources)

	# Verify all interaction terms are present with correct df
	for (nm in names(expected_interactions)) {
		expect_true(nm %in% names(df_map))
		expect_equal(df_map[[nm]], unname(expected_interactions[[nm]]))
	}
})

test_that("get_anova_structure throws error for unknown design type", {
	# Create a minimal design book
	design_book <- data.frame(
		plot = 1:4,
		treatment = c("A", "B", "A", "B")
	)

	# Test that unknown design type triggers error
	expect_error(
		biometryassist:::get_anova_structure("unknown_design", design_book),
		"Unknown design type: unknown_design"
	)

	expect_error(
		biometryassist:::get_anova_structure("invalid", design_book),
		"Unknown design type: invalid"
	)
})

test_that("anova_split handles different subplot column names", {
	# Test with 'subplots' column name
	design_book_subplots <- data.frame(
		plots = 1:8,
		block = rep(1:2, each = 4),
		wholeplots = rep(1:4, each = 2),
		subplots = rep(1:2, 4),
		TreatmentA = rep(c("A1", "A2"), each = 4),
		TreatmentB = rep(c("B1", "B2"), 4)
	)

	result1 <- biometryassist:::anova_split(design_book_subplots)
	expect_type(result1, "list")
	expect_true("sources" %in% names(result1))
	expect_true("df" %in% names(result1))

	# Test with 'splots' column name
	design_book_splots <- data.frame(
		plots = 1:8,
		block = rep(1:2, each = 4),
		wholeplots = rep(1:4, each = 2),
		splots = rep(1:2, 4),
		TreatmentA = rep(c("A1", "A2"), each = 4),
		TreatmentB = rep(c("B1", "B2"), 4)
	)

	result2 <- biometryassist:::anova_split(design_book_splots)
	expect_type(result2, "list")
	expect_true("sources" %in% names(result2))
	expect_true("df" %in% names(result2))

	# Results should be identical (same df structure)
	expect_equal(result1$df, result2$df)
})

test_that("anova_split throws error when subplot column is missing", {
	# Design book without subplots or splots column
	design_book_invalid <- data.frame(
		plots = 1:8,
		block = rep(1:2, each = 4),
		wholeplots = rep(1:4, each = 2),
		TreatmentA = rep(c("A1", "A2"), each = 4),
		TreatmentB = rep(c("B1", "B2"), 4)
	)

	expect_error(
		biometryassist:::anova_split(design_book_invalid),
		"Cannot find subplot column in design book"
	)
})

test_that("anova_split throws error when wrong number of treatment columns", {
	# Design book with only 1 treatment column
	design_book_one_trt <- data.frame(
		plots = 1:8,
		block = rep(1:2, each = 4),
		wholeplots = rep(1:4, each = 2),
		subplots = rep(1:2, 4),
		TreatmentA = rep(c("A1", "A2"), each = 4)
	)

	expect_error(
		biometryassist:::anova_split(design_book_one_trt),
		"Expected 2 treatment columns in split plot design, found 1"
	)

	# Design book with 3 treatment columns
	design_book_three_trt <- data.frame(
		plots = 1:8,
		block = rep(1:2, each = 4),
		wholeplots = rep(1:4, each = 2),
		subplots = rep(1:2, 4),
		TreatmentA = rep(c("A1", "A2"), each = 4),
		TreatmentB = rep(c("B1", "B2"), 4),
		TreatmentC = rep(c("C1", "C2"), 4)
	)

	expect_error(
		biometryassist:::anova_split(design_book_three_trt),
		"Expected 2 treatment columns in split plot design, found 3"
	)
})

test_that("anova_strip throws error when block column is missing", {
	design_book_missing_block <- data.frame(
		plots = 1:4,
		wholeplots = 1:4,
		subplots = rep(1:2, 2),
		TreatmentA = rep(c("A1", "A2"), each = 2),
		TreatmentB = rep(c("B1", "B2"), 2)
	)

	expect_error(
		biometryassist:::anova_strip(design_book_missing_block),
		"Expected a 'block' column in strip plot design"
	)
})

test_that("anova_strip throws error when wrong number of treatment columns", {
	# Only 1 treatment column
	design_book_one_trt <- data.frame(
		plots = 1:4,
		block = rep(1:2, each = 2),
		wholeplots = 1:4,
		subplots = rep(1:2, 2),
		TreatmentA = rep(c("A1", "A2"), each = 2)
	)

	expect_error(
		biometryassist:::anova_strip(design_book_one_trt),
		"Expected 2 treatment columns in strip plot design, found 1"
	)

	# 3 treatment columns
	design_book_three_trt <- data.frame(
		plots = 1:4,
		block = rep(1:2, each = 2),
		wholeplots = 1:4,
		subplots = rep(1:2, 2),
		TreatmentA = rep(c("A1", "A2"), each = 2),
		TreatmentB = rep(c("B1", "B2"), 2),
		TreatmentC = rep(c("C1", "C2"), 2)
	)

	expect_error(
		biometryassist:::anova_strip(design_book_three_trt),
		"Expected 2 treatment columns in strip plot design, found 3"
	)
})
