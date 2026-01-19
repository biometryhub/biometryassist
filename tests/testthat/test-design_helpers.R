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
