# construct_factorial_labels() tests ----

test_that("construct_factorial_labels works with default separators", {
    # Create a simple design_book with 2 factors
    design_book <- data.frame(
        plot = 1:4,
        block = 1,
        A = c("a1", "a1", "a2", "a2"),
        B = c("b1", "b2", "b1", "b2")
    )

    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 3)

    # Default separators: c("", " ") means no separator between name and level, space between factors
    expect_equal(as.character(result), c("Aa1 Bb1", "Aa1 Bb2", "Aa2 Bb1", "Aa2 Bb2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels works with single custom separator", {
    # Create a simple design_book
    design_book <- data.frame(
        plot = 1:2,
        A = c("low", "high"),
        B = c("x", "y")
    )

    # When a single separator is provided, it's used for both positions
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2, fac.sep = "_")

    expect_equal(as.character(result), c("A_low_B_x", "A_high_B_y"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels works with two custom separators", {
    # Create a design_book
    design_book <- data.frame(
        id = 1:2,
        Variety = c("V1", "V2"),
        Fertilizer = c("F1", "F2")
    )

    # First separator between name and level, second between factors
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2, fac.sep = c(":", " x "))

    expect_equal(as.character(result), c("Variety:V1 x Fertilizer:F1", "Variety:V2 x Fertilizer:F2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels handles single factor column", {
    # Design book with only one factor column to combine
    design_book <- data.frame(
        plot = 1:3,
        treatment = c("T1", "T2", "T3")
    )

    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2)

    expect_equal(as.character(result), c("treatmentT1", "treatmentT2", "treatmentT3"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels handles three factors", {
    # Design book with three factors
    design_book <- data.frame(
        A = c("a1", "a2"),
        B = c("b1", "b2"),
        C = c("c1", "c2")
    )

    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c("=", ", "))

    expect_equal(as.character(result), c("A=a1, B=b1, C=c1", "A=a2, B=b2, C=c2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels trims whitespace from results", {
    # Design book that might produce extra whitespace
    design_book <- data.frame(
        X = c("val1", "val2"),
        Y = c("val3", "val4")
    )

    # Using space as both separators could create extra spaces
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c(" ", " "))

    # Check that there's no leading/trailing whitespace
    expect_equal(as.character(result), c("X val1 Y val3", "X val2 Y val4"))
    expect_true(all(!grepl("^\\s|\\s$", as.character(result))))
})

test_that("construct_factorial_labels throws error for invalid start_col", {
    design_book <- data.frame(
        A = c("a1", "a2"),
        B = c("b1", "b2")
    )

    # start_col greater than ncol(design_book) should error
    expect_error(
        biometryassist:::construct_factorial_labels(design_book, start_col = 5),
        "start_col must be <= ncol\\(design_book\\)"
    )
})

test_that("construct_factorial_labels handles various data types in columns", {
    # Design book with numeric and character columns
    design_book <- data.frame(
        dose = c(10, 20, 30),
        method = c("A", "B", "C"),
        temp = c(20, 25, 30)
    )

    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c(":", "|"))

    expect_equal(as.character(result), c("dose:10|method:A|temp:20", "dose:20|method:B|temp:25", "dose:30|method:C|temp:30"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels produces correct factor levels", {
    # Create design with repeated combinations
    design_book <- data.frame(
        A = c("a1", "a2", "a1", "a2"),
        B = c("b1", "b1", "b2", "b2")
    )

    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1)

    # Should have 4 treatments
    expect_length(result, 4)
    # Should have 4 unique levels
    expect_equal(nlevels(result), 4)
    # Check the actual levels
    expect_setequal(levels(result), c("Aa1 Bb1", "Aa1 Bb2", "Aa2 Bb1", "Aa2 Bb2"))
})

test_that("construct_factorial_labels works with empty string separators", {
    design_book <- data.frame(
        Factor1 = c("X", "Y"),
        Factor2 = c("1", "2")
    )

    # Both separators as empty strings
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c("", ""))

    expect_equal(as.character(result), c("Factor1XFactor21", "Factor1YFactor22"))
    expect_s3_class(result, "factor")
})

# Tests for apply_factor_names() internal function

# Helper to access internal function
apply_factor_names <- biometryassist:::apply_factor_names

# Test 1: NULL fac.names returns design_book unchanged ----
test_that("apply_factor_names returns design_book unchanged when fac.names is NULL", {
  design_book <- data.frame(
    A = factor(c("A1", "A2")),
    B = factor(c("B1", "B2")),
    plot = 1:2
  )

  result <- apply_factor_names(design_book, fac.names = NULL, design_type = "factorial")

  expect_identical(result, design_book)
})

# Test 2: Unknown design_type throws error ----
test_that("apply_factor_names throws error for unknown design_type", {
  design_book <- data.frame(
    A = factor(c("A1", "A2")),
    B = factor(c("B1", "B2"))
  )

  expect_error(
    apply_factor_names(design_book, fac.names = list(F1 = c("a", "b"), F2 = c("x", "y")), design_type = "unknown"),
    "Unknown design_type: unknown"
  )
})

# Test 3: Factorial design with 2 factors ----
test_that("apply_factor_names works with 2-factor factorial design", {
  design_book <- data.frame(
    A = factor(c("1", "2", "1", "2")),
    B = factor(c("X", "X", "Y", "Y")),
    plot = 1:4
  )

  fac.names <- list(
    Nitrogen = c("Low", "High"),
    Water = c("Dry", "Wet")
  )

  result <- apply_factor_names(design_book, fac.names, design_type = "factorial")

  # Check that column names changed
  expect_true("Nitrogen" %in% colnames(result))
  expect_true("Water" %in% colnames(result))
  expect_false("A" %in% colnames(result))
  expect_false("B" %in% colnames(result))

  # Check that factor levels changed
  expect_equal(levels(result$Nitrogen), c("Low", "High"))
  expect_equal(levels(result$Water), c("Dry", "Wet"))
})

# Test 4: Factorial design with 3 factors ----
test_that("apply_factor_names works with 3-factor factorial design", {
  design_book <- data.frame(
    A = factor(c("1", "2")),
    B = factor(c("X", "Y")),
    C = factor(c("P", "Q")),
    plot = 1:2
  )

  fac.names <- list(
    Nitrogen = c("N50", "N100"),
    Water = c("Irrigated", "Rainfed"),
    Variety = c("V1", "V2")
  )

  result <- apply_factor_names(design_book, fac.names, design_type = "factorial")

  # Check that column names changed
  expect_true("Nitrogen" %in% colnames(result))
  expect_true("Water" %in% colnames(result))
  expect_true("Variety" %in% colnames(result))

  # Check that factor levels changed
  expect_equal(levels(result$Nitrogen), c("N50", "N100"))
  expect_equal(levels(result$Water), c("Irrigated", "Rainfed"))
  expect_equal(levels(result$Variety), c("V1", "V2"))
})

# Test 5: Split plot design with list ----
test_that("apply_factor_names works with split plot design using list", {
  design_book <- data.frame(
    treatments = factor(c("A", "B", "A", "B")),
    sub_treatments = factor(c("1", "2", "1", "2")),
    plot = 1:4
  )

  fac.names <- list(
    Water = c("Irrigated", "Rainfed"),
    Nitrogen = c("N50", "N100")
  )

  result <- apply_factor_names(design_book, fac.names, design_type = "split")

  # Check that column names changed
  expect_true("Water" %in% colnames(result))
  expect_true("Nitrogen" %in% colnames(result))
  expect_false("treatments" %in% colnames(result))
  expect_false("sub_treatments" %in% colnames(result))

  # Check that factor levels changed
  expect_equal(levels(result$Water), c("Irrigated", "Rainfed"))
  expect_equal(levels(result$Nitrogen), c("N50", "N100"))
})

# Test 6: Split plot design with character vector ----
test_that("apply_factor_names works with split plot design using character vector", {
  design_book <- data.frame(
    treatments = factor(c("A", "B")),
    sub_treatments = factor(c("1", "2")),
    plot = 1:2
  )

  fac.names <- c("Water", "Nitrogen")

  result <- apply_factor_names(design_book, fac.names, design_type = "split")

  # Check that column names changed
  expect_true("Water" %in% colnames(result))
  expect_true("Nitrogen" %in% colnames(result))
  expect_false("treatments" %in% colnames(result))
  expect_false("sub_treatments" %in% colnames(result))
})

# Test 7: Warning when fac.names has too many elements ----
test_that("apply_factor_names warns when fac.names has too many elements for factorial", {
  design_book <- data.frame(
    A = factor(c("1", "2")),
    B = factor(c("X", "Y")),
    plot = 1:2
  )

  fac.names <- list(
    F1 = c("a", "b"),
    F2 = c("x", "y"),
    F3 = c("p", "q")  # Too many
  )

  expect_warning(
    apply_factor_names(design_book, fac.names, design_type = "factorial"),
    "fac.names contains 3 elements but only the first 2 have been used."
  )
})

# Test 8: Warning when fac.names has too few elements ----
test_that("apply_factor_names warns and returns unchanged when fac.names has too few elements", {
  design_book <- data.frame(
    A = factor(c("1", "2")),
    B = factor(c("X", "Y")),
    plot = 1:2
  )

  fac.names <- list(
    F1 = c("a", "b")  # Only 1, need 2
  )

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial"),
    "fac.names doesn't contain enough elements and has not been used."
  )

  # Check that design_book is returned unchanged
  expect_identical(result, design_book)
})

# Test 9: Warning when factor levels don't match ----
test_that("apply_factor_names warns when factor levels don't match", {
  design_book <- data.frame(
    A = factor(c("1", "2", "3")),  # 3 levels
    B = factor(c("X", "Y", "Y")),
    plot = 1:3
  )

  fac.names <- list(
    Nitrogen = c("Low", "High"),  # Only 2 levels, but A has 3
    Water = c("Dry", "Wet")
  )

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial"),
    "Nitrogen must contain the correct number of elements. Elements have not been applied."
  )

  # Check that the mismatched factor was NOT changed
  expect_true("A" %in% colnames(result))  # Column name should still be A
  expect_false("Nitrogen" %in% colnames(result))

  # Check that the correctly matched factor WAS changed
  expect_true("Water" %in% colnames(result))
  expect_equal(levels(result$Water), c("Dry", "Wet"))
})

# Test 10: Split plot with too few elements ----
test_that("apply_factor_names warns for split plot with too few elements", {
  design_book <- data.frame(
    treatments = factor(c("A", "B")),
    sub_treatments = factor(c("1", "2")),
    plot = 1:2
  )

  fac.names <- list(Water = c("Irrigated", "Rainfed"))  # Only 1, need 2

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "split"),
    "fac.names doesn't contain enough elements and has not been used."
  )

  # Check that design_book is returned unchanged
  expect_identical(result, design_book)
})

# Test 11: Split plot with too many elements ----
test_that("apply_factor_names warns for split plot with too many elements", {
  design_book <- data.frame(
    treatments = factor(c("A", "B")),
    sub_treatments = factor(c("1", "2")),
    plot = 1:2
  )

  fac.names <- list(
    Water = c("Irrigated", "Rainfed"),
    Nitrogen = c("N50", "N100"),
    Extra = c("E1", "E2")  # Too many
  )

  expect_warning(
    apply_factor_names(design_book, fac.names, design_type = "split"),
    "fac.names contains 3 elements but only the first 2 have been used."
  )
})

# Test 12: Factorial design with partial mismatch in levels ----
test_that("apply_factor_names handles partial mismatch in factor levels for factorial", {
  design_book <- data.frame(
    A = factor(c("1", "2", "2")),
    B = factor(c("X", "Y", "Z")),  # 3 levels
    plot = 1:3
  )

  fac.names <- list(
    Nitrogen = c("Low", "High"),  # Matches A (2 levels)
    Water = c("Dry", "Wet")       # Doesn't match B (3 levels)
  )

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial"),
    "Water must contain the correct number of elements. Elements have not been applied."
  )

  # Check that Nitrogen was applied successfully
  expect_true("Nitrogen" %in% colnames(result))
  expect_equal(levels(result$Nitrogen), c("Low", "High"))

  # Check that Water was not applied
  expect_true("B" %in% colnames(result))
  expect_false("Water" %in% colnames(result))
})

# Test 13: Split plot with mismatched levels ----
test_that("apply_factor_names warns when split plot factor levels don't match", {
  design_book <- data.frame(
    treatments = factor(c("A", "B", "C")),  # 3 levels
    sub_treatments = factor(c("1", "2", "2")),
    plot = 1:3
  )

  fac.names <- list(
    Water = c("Irrigated", "Rainfed"),  # Only 2, but treatments has 3
    Nitrogen = c("N50", "N100")
  )

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "split"),
    "Water must contain the correct number of elements. Elements have not been applied."
  )
})

# Test 14: Factorial design - column name updates only on successful application ----
test_that("apply_factor_names only updates column names when levels are successfully applied", {
  design_book <- data.frame(
    A = factor(c("1", "2", "2")),
    B = factor(c("X", "Y", "Z")),  # Mismatch
    plot = 1:3
  )

  fac.names <- list(
    GoodName = c("a", "b"),      # Matches A
    BadName = c("x", "y")        # Doesn't match B
  )

  expect_warning(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial"),
    "BadName must contain the correct number of elements"
  )

  # GoodName should be applied to A
  expect_true("GoodName" %in% colnames(result))
  expect_false("A" %in% colnames(result))

  # BadName should NOT be applied to B
  expect_true("B" %in% colnames(result))
  expect_false("BadName" %in% colnames(result))
})

# Test 15: Verify factorial design determines n_facs correctly for 2 factors ----
test_that("apply_factor_names correctly identifies 2-factor factorial design", {
  # Design with A and B but no C
  design_book <- data.frame(
    A = factor(c("1", "2")),
    B = factor(c("X", "Y"))
  )

  fac.names <- list(
    F1 = c("a", "b"),
    F2 = c("x", "y")
  )

  # Should work without error
  expect_silent(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial")
  )

  expect_true("F1" %in% colnames(result))
  expect_true("F2" %in% colnames(result))
})

# Test 16: Verify factorial design determines n_facs correctly for 3 factors ----
test_that("apply_factor_names correctly identifies 3-factor factorial design", {
  # Design with A, B, and C
  design_book <- data.frame(
    A = factor(c("1", "2")),
    B = factor(c("X", "Y")),
    C = factor(c("P", "Q"))
  )

  fac.names <- list(
    F1 = c("a", "b"),
    F2 = c("x", "y"),
    F3 = c("p", "q")
  )

  # Should work without error
  expect_silent(
    result <- apply_factor_names(design_book, fac.names, design_type = "factorial")
  )

  expect_true("F1" %in% colnames(result))
  expect_true("F2" %in% colnames(result))
  expect_true("F3" %in% colnames(result))
})
# Tests for calculate_block_layout() helper function

test_that("calculate_block_layout validates brows and bcols parameters", {
  # NULL values
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, NULL, 5, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 5, NULL, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )

  # NA values
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, NA, 5, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 5, NA, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )

  # Infinite values
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, Inf, 5, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 5, Inf, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )

  # Zero or negative values
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 0, 5, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, -1, 5, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 5, 0, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
  expect_error(
    biometryassist:::calculate_block_layout(10, 10, 5, -1, 10),
    "calculate_block_layout: 'brows' and 'bcols' must be positive, finite, non-missing values."
  )
})

test_that("calculate_block_layout handles blocking across rows (brows == ntrt)", {
  # When brows equals ntrt, blocks span entire columns
  result <- biometryassist:::calculate_block_layout(
    nrows = 6, ncols = 3, brows = 6, bcols = 1, ntrt = 6
  )

  # Should return expand.grid(row = 1:nrows, col = 1:ncols)
  expected <- expand.grid(row = 1:6, col = 1:3)
  expect_equal(result, expected)
  expect_equal(nrow(result), 18)
  expect_equal(names(result), c("row", "col"))

  # Check that rows are ordered correctly (1,2,3,4,5,6 for each column)
  expect_equal(result$row[1:6], 1:6)
  expect_equal(result$col[1:6], rep(1, 6))
})

test_that("calculate_block_layout handles blocking incomplete rows with all columns (rr > 1 & cc == 1)", {
  # Multiple row blocks spanning all columns
  result <- biometryassist:::calculate_block_layout(
    nrows = 8, ncols = 4, brows = 2, bcols = 4, ntrt = 8
  )

  # Should return expand.grid(col = 1:ncols, row = 1:nrows)
  # Note the order is col first, then row
  expected <- expand.grid(col = 1:4, row = 1:8)
  expect_equal(result, expected)
  expect_equal(nrow(result), 32)
  expect_equal(names(result), c("col", "row"))

  # Check that columns are ordered correctly (1,2,3,4 for each row)
  expect_equal(result$col[1:4], 1:4)
  expect_equal(result$row[1:4], rep(1, 4))
})

test_that("calculate_block_layout handles blocking across columns (bcols == ntrt)", {
  # When bcols equals ntrt, blocks span entire rows
  result <- biometryassist:::calculate_block_layout(
    nrows = 3, ncols = 6, brows = 1, bcols = 6, ntrt = 6
  )

  # Should return expand.grid(col = 1:ncols, row = 1:nrows)
  expected <- expand.grid(col = 1:6, row = 1:3)
  expect_equal(result, expected)
  expect_equal(nrow(result), 18)
  expect_equal(names(result), c("col", "row"))

  # Check ordering
  expect_equal(result$col[1:6], 1:6)
  expect_equal(result$row[1:6], rep(1, 6))
})

test_that("calculate_block_layout handles blocking incomplete rows and columns (rr > 1 & cc > 1)", {
  # Square blocks with multiple rows and columns
  # Example: 6x4 grid with 3x2 blocks (2 row blocks, 2 column blocks = 4 blocks)
  block_vec <- rep(1:4, each = 6)
  result <- biometryassist:::calculate_block_layout(
    nrows = 6, ncols = 4, brows = 3, bcols = 2, ntrt = 6, block_vec = block_vec
  )

  expect_equal(nrow(result), 24)
  expect_equal(names(result), c("row", "col"))
  expect_false(any(is.na(result$row)))
  expect_false(any(is.na(result$col)))

  # All row and column values should be within bounds
  expect_true(all(result$row >= 1 & result$row <= 6))
  expect_true(all(result$col >= 1 & result$col <= 4))

  # Check that block assignments produced correct layout
  # Block 1 should be in rows 1-3, cols 1-2
  block1_rows <- result$row[1:6]
  block1_cols <- result$col[1:6]
  expect_true(all(block1_rows >= 1 & block1_rows <= 3))
  expect_true(all(block1_cols >= 1 & block1_cols <= 2))
})

test_that("calculate_block_layout handles blocking incomplete columns with all rows (cc > 1 & rr == 1)", {
  # Multiple column blocks spanning all rows
  block_vec <- rep(1:3, each = 4)
  result <- biometryassist:::calculate_block_layout(
    nrows = 4, ncols = 6, brows = 4, bcols = 2, ntrt = 8, block_vec = block_vec
  )

  expect_equal(nrow(result), 24)
  expect_equal(names(result), c("row", "col"))
  expect_false(any(is.na(result$row)))
  expect_false(any(is.na(result$col)))

  # All row and column values should be within bounds
  expect_true(all(result$row >= 1 & result$row <= 4))
  expect_true(all(result$col >= 1 & result$col <= 6))

  # Block 1 should be in all rows (1-4), cols 1-2
  block1_rows <- result$row[1:4]
  block1_cols <- result$col[1:4]
  expect_true(all(block1_rows >= 1 & block1_rows <= 4))
  expect_true(all(block1_cols >= 1 & block1_cols <= 2))
})

test_that("calculate_block_layout falls back to default for edge cases", {
  # Test the default fallback when none of the special conditions are met
  # This can happen when rr == 1 and cc == 1 (single block)
  result <- biometryassist:::calculate_block_layout(
    nrows = 5, ncols = 5, brows = 5, bcols = 5, ntrt = 10
  )

  # Should return expand.grid(row = 1:nrows, col = 1:ncols)
  expected <- expand.grid(row = 1:5, col = 1:5)
  expect_equal(result, expected)
  expect_equal(nrow(result), 25)
  expect_equal(names(result), c("row", "col"))
})

test_that("calculate_block_layout handles various block configurations correctly", {
  # Test 1: 4x3 grid with 2x1 blocks (2 row blocks, 3 column blocks)
  block_vec <- rep(1:6, each = 2)
  result1 <- biometryassist:::calculate_block_layout(
    nrows = 4, ncols = 3, brows = 2, bcols = 1, ntrt = 6, block_vec = block_vec
  )
  expect_equal(nrow(result1), 12)
  expect_equal(names(result1), c("row", "col"))
  expect_true(all(result1$row >= 1 & result1$row <= 4))
  expect_true(all(result1$col >= 1 & result1$col <= 3))

  # Test 2: 3x4 grid with 1x2 blocks (3 row blocks, 2 column blocks)
  block_vec <- rep(1:6, each = 2)
  result2 <- biometryassist:::calculate_block_layout(
    nrows = 3, ncols = 4, brows = 1, bcols = 2, ntrt = 6, block_vec = block_vec
  )
  expect_equal(nrow(result2), 12)
  expect_equal(names(result2), c("row", "col"))
  expect_true(all(result2$row >= 1 & result2$row <= 3))
  expect_true(all(result2$col >= 1 & result2$col <= 4))
})

test_that("calculate_block_layout returns correct dimensions for all paths", {
  # Each test should return nrows * ncols total positions

  # Path 1: brows == ntrt
  r1 <- biometryassist:::calculate_block_layout(5, 4, 5, 1, 5)
  expect_equal(nrow(r1), 20)

  # Path 2: rr > 1 & cc == 1
  r2 <- biometryassist:::calculate_block_layout(6, 3, 2, 3, 6)
  expect_equal(nrow(r2), 18)

  # Path 3: bcols == ntrt
  r3 <- biometryassist:::calculate_block_layout(3, 5, 1, 5, 5)
  expect_equal(nrow(r3), 15)

  # Path 4: rr > 1 & cc > 1 (with block_vec)
  r4 <- biometryassist:::calculate_block_layout(6, 6, 3, 3, 9, rep(1:4, each = 9))
  expect_equal(nrow(r4), 36)

  # Path 5: cc > 1 & rr == 1 (with block_vec)
  r5 <- biometryassist:::calculate_block_layout(3, 6, 3, 2, 6, rep(1:3, each = 6))
  expect_equal(nrow(r5), 18)

  # Path 6: default
  r6 <- biometryassist:::calculate_block_layout(4, 4, 4, 4, 8)
  expect_equal(nrow(r6), 16)
})

# Tests for internal helper functions in design_helpers.R

test_that("get_design_info correctly identifies non-factorial designs", {
  # Test with a CRD design
  crd_design <- agricolae::design.crd(trt = c("A", "B", "C"), r = 3, seed = 42)
  result <- biometryassist:::get_design_info(crd_design)

  expect_type(result, "list")
  expect_named(result, c("type", "is_factorial", "base"))
  expect_equal(result$type, "crd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info correctly identifies RCBD designs", {
  # Test with an RCBD design
  rcbd_design <- agricolae::design.rcbd(trt = c("T1", "T2", "T3", "T4"),
                                        r = 4, seed = 42)
  result <- biometryassist:::get_design_info(rcbd_design)

  expect_type(result, "list")
  expect_equal(result$type, "rcbd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "rcbd")
})

test_that("get_design_info correctly identifies LSD designs", {
  # Test with a Latin Square design
  lsd_design <- agricolae::design.lsd(trt = c("V1", "V2", "V3", "V4"),
                                      seed = 42)
  result <- biometryassist:::get_design_info(lsd_design)

  expect_type(result, "list")
  expect_equal(result$type, "lsd")
  expect_false(result$is_factorial)
  expect_equal(result$base, "lsd")
})

test_that("get_design_info correctly identifies factorial designs with CRD", {
  # Test with a factorial CRD design
  factorial_crd <- agricolae::design.ab(trt = c(2, 3), r = 2,
                                        design = "crd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_crd)

  expect_type(result, "list")
  expect_named(result, c("type", "is_factorial", "base"))
  expect_equal(result$type, "factorial_crd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info correctly identifies factorial designs with RCBD", {
  # Test with a factorial RCBD design
  factorial_rcbd <- agricolae::design.ab(trt = c(3, 2), r = 3,
                                         design = "rcbd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_rcbd)

  expect_type(result, "list")
  expect_equal(result$type, "factorial_rcbd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "rcbd")
})

test_that("get_design_info correctly identifies factorial designs with LSD", {
  # Test with a factorial Latin Square design
  factorial_lsd <- agricolae::design.ab(trt = c(2, 2), r = 1,
                                        design = "lsd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_lsd)

  expect_type(result, "list")
  expect_equal(result$type, "factorial_lsd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "lsd")
})

test_that("get_design_info handles 3-way factorial designs", {
  # Test with a 3-way factorial design
  factorial_3way <- agricolae::design.ab(trt = c(2, 2, 2), r = 2,
                                         design = "crd", seed = 42)
  result <- biometryassist:::get_design_info(factorial_3way)

  expect_type(result, "list")
  expect_equal(result$type, "factorial_crd")
  expect_true(result$is_factorial)
  expect_equal(result$base, "crd")
})

test_that("get_design_info structure is consistent across design types", {
  # Create different design types and verify structure is always the same
  designs <- list(
    crd = agricolae::design.crd(trt = c("A", "B"), r = 2, seed = 42),
    rcbd = agricolae::design.rcbd(trt = c("A", "B"), r = 3, seed = 42),
    lsd = agricolae::design.lsd(trt = c("A", "B", "C"), seed = 42),
    factorial = agricolae::design.ab(trt = c(2, 2), r = 2, design = "crd", seed = 42)
  )

  for (design in designs) {
    result <- biometryassist:::get_design_info(design)

    # Check that all expected fields are present
    expect_named(result, c("type", "is_factorial", "base"))

    # Check types of fields
    expect_type(result$type, "character")
    expect_type(result$is_factorial, "logical")
    expect_type(result$base, "character")

    # Check that all fields have length 1
    expect_length(result$type, 1)
    expect_length(result$is_factorial, 1)
    expect_length(result$base, 1)
  }
})
