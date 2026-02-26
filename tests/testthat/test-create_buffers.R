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
  expect_error(create_buffers(design, type = "block"), "Block buffers are not yet supported")
  expect_error(create_buffers(design, type = "notatype"), "Invalid buffer option")
})

test_that("add_buffers works for design objects", {
  design_obj <- design(type = "crd", treatments = c(1, 5, 10, 20),
                       reps = 5, nrows = 4, ncols = 5, seed = 42, quiet = TRUE)
  out <- add_buffers(design_obj, type = "edge")
  expect_s3_class(out, "design")
  expect_true(any(out$design$treatments == "buffer"))
})
