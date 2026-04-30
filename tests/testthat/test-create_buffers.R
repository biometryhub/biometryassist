test_that("create_buffers adds edge buffers correctly", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  out <- create_buffers(design, type = "edge")
  expect_true(any(out$treatments == "buffer"))
  expect_true(all(c(1, max(out$row)) %in% out$row))
  expect_true(all(c(1, max(out$col)) %in% out$col))
})

test_that("create_buffers adds row buffers correctly", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  out <- create_buffers(design, type = "row")
  expect_true(any(out$treatments == "buffer"))
  expect_true(all(seq(1, max(out$row), by = 2) %in% out$row))
})

test_that("create_buffers adds column buffers correctly", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  out <- create_buffers(design, type = "col")
  expect_true(any(out$treatments == "buffer"))
  expect_true(all(seq(1, max(out$col), by = 2) %in% out$col))
})

test_that("create_buffers adds double row buffers correctly", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  out <- create_buffers(design, type = "double row")
  expect_true(any(out$treatments == "buffer"))
})

test_that("create_buffers adds double column buffers correctly", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  out <- create_buffers(design, type = "double col")
  expect_true(any(out$treatments == "buffer"))
})

test_that("create_buffers errors for unsupported buffer type", {
  design <- data.frame(row = rep(1:2, each = 2),
                       col = rep(1:2, 2),
                       treatments = c("A", "B", "C", "D"))
  expect_error(create_buffers(design, type = "block", blocks = TRUE), "Block buffers require a 'block' column")
  expect_error(create_buffers(design, type = "db", blocks = TRUE), "Block buffers require a 'block' column")
  expect_error(create_buffers(design, type = "notatype"), "Invalid buffer option")
})

test_that("create_buffers adds block buffers correctly", {
  design <- data.frame(
    row = rep(1:4, each = 2),
    col = rep(1:2, 4),
    treatments = paste0("T", seq_len(8)),
    block = rep(c(1, 2), each = 4)
  )

  out <- create_buffers(design, type = "block", blocks = TRUE)

  expect_true(any(out$treatments == "buffer"))
  expect_true(any(is.na(out$block)))

  # Should expand rows to make room for buffers
  expect_true(max(out$row) > max(design$row))
  expect_true(max(out$col) == max(design$col))

  # No duplicated row/col coordinates
  expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("create_buffers adds internal block buffers along column boundaries", {
  # Two blocks split left/right to force a column boundary.
  design <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatments = paste0("T", seq_len(4)),
    block = c(1, 2, 1, 2)
  )

  out <- create_buffers(design, type = "block", blocks = TRUE)

  expect_true(any(out$treatments == "buffer"))
  # Internal boundary buffers should have NA block values
  expect_true(any(out$treatments == "buffer" & is.na(out$block)))
  # Column boundary insertion should expand columns
  expect_true(max(out$col) > max(design$col))
  # No duplicated row/col coordinates
  expect_equal(nrow(out), nrow(unique(out[c("row", "col")])) )
})

test_that("create_buffers handles pure column boundary (1x2 grid)", {
  # Minimal case: one row, two columns, different blocks left/right.
  # Guarantees col_boundary_gaps is non-empty (hits col_gap_counts[...] <- 1L).
  design <- data.frame(
    row = c(1, 1),
    col = c(1, 2),
    treatments = c("A", "B"),
    block = c(1, 2)
  )

  out <- create_buffers(design, type = "block", blocks = TRUE)

  expect_true(any(out$treatments == "buffer"))
  expect_true(any(out$treatments == "buffer" & is.na(out$block)))
  expect_true(max(out$col) > max(design$col))
  expect_equal(nrow(out), nrow(unique(out[c("row", "col")])) )
})

test_that("create_buffers returns design unchanged when no internal boundaries", {
  # All plots are in the same block; there are no internal boundaries, so
  # buffers_rc is empty and the function returns early.
  design <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, times = 2),
    treatments = c("A", "B", "C", "D"),
    block = 1
  )

  out <- create_buffers(design, type = "block", blocks = TRUE)

  expect_false(any(out$treatments == "buffer"))
  expect_equal(out$row, design$row)
  expect_equal(out$col, design$col)
  expect_equal(out$block, design$block)
  expect_true(is.factor(out$treatments))
  expect_true("buffer" %in% levels(out$treatments))
})

test_that("create_buffers adds double/entire/full block buffers (numeric block)", {
  # 4 blocks arranged in a 2x2 grid to exercise both:
  # - blocks on the outer edge (gap_k < row_min_all/col_min_all branches)
  # - internal adjacency (double-inserted separator rows/cols)
  grid <- expand.grid(row = 1:4, col = 1:4)
  grid$block <- with(grid, ifelse(row <= 2 & col <= 2, 1,
                          ifelse(row <= 2 & col >= 3, 2,
                          ifelse(row >= 3 & col <= 2, 3, 4))))
  grid$treatments <- paste0("T", seq_len(nrow(grid)))

  type_aliases <- c("double blocks", "entire block", "full blocks", "db", "eb", "fb")
  for (tp in type_aliases) {
    out <- create_buffers(grid, type = tp, blocks = TRUE)

    expect_true(any(out$treatments == "buffer"))
    expect_false(any(is.na(out$block)))

    # Should expand both rows and cols to make room for the surrounding rings
    expect_true(max(out$row) > max(grid$row))
    expect_true(max(out$col) > max(grid$col))

    # No duplicated row/col coordinates
    expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
  }
})

test_that("create_buffers adds double/entire/full block buffers (factor block, factor treatments)", {
  grid <- expand.grid(row = 1:4, col = 1:4)
  grid$block <- factor(with(grid, ifelse(row <= 2 & col <= 2, 1,
                                 ifelse(row <= 2 & col >= 3, 2,
                                 ifelse(row >= 3 & col <= 2, 3, 4))))
  )

  # Treatments already factor and already include buffer level: exercises the
  # "skip" paths for the factor/level checks.
  trt_levels <- c(paste0("T", seq_len(nrow(grid))), "buffer")
  grid$treatments <- factor(paste0("T", seq_len(nrow(grid))), levels = trt_levels)

  out <- create_buffers(grid, type = "double block", blocks = TRUE)
  expect_true(any(out$treatments == "buffer"))
  expect_false(any(is.na(out$block)))
  expect_true(max(out$row) > max(grid$row))
  expect_true(max(out$col) > max(grid$col))
  expect_equal(nrow(out), nrow(unique(out[c("row", "col")])))
})

test_that("add_buffers works for design objects", {
  design_obj <- design(type = "crd", treatments = c(1, 5, 10, 20),
                       reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
  out <- add_buffers(design_obj, type = "edge")
  expect_s3_class(out, "design")
  expect_true(any(out$design$treatments == "buffer"))
})

test_that("add_buffers works for block buffers", {
  design_obj <- design(type = "rcbd", treatments = LETTERS[1:4],
                       reps = 2, nrows = 4, ncols = 2, brows = 2, bcols = 2,
                       seed = 42, quiet = TRUE)
  out <- add_buffers(design_obj, type = "block")
  expect_s3_class(out, "design")
  expect_true(any(out$design$treatments == "buffer"))
})
