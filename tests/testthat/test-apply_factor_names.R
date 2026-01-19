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
    B = factor(c("X", "Y")),
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
    A = factor(c("1", "2")),
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
    sub_treatments = factor(c("1", "2")),
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
    A = factor(c("1", "2")),
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
