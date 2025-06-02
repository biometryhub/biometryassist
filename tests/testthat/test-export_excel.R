test_that("design_to_excel_layout returns correct layout matrix", {
  df <- data.frame(
    row = rep(1:2, each = 3),
    col = rep(1:3, 2),
    treatments = c("A", "B", "C", "C", "B", "A")
  )
  layout <- design_to_excel_layout(df, value_column = "treatments")
  expect_s3_class(layout, "data.frame")
  expect_equal(dim(layout), c(2, 3))
  expect_equal(layout[1, 1], "A")
  expect_equal(layout[2, 3], "A")
})

test_that("design_to_excel_layout errors if required columns are missing", {
  df <- data.frame(row = 1:2, treatments = c("A", "B"))
  expect_error(design_to_excel_layout(df, value_column = "treatments"),
               "Missing required columns")
})

test_that("design_to_excel_layout works with a list input", {
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  design_list <- list(design = df)
  layout <- design_to_excel_layout(design_list, value_column = "treatments")
  expect_s3_class(layout, "data.frame")
  expect_equal(dim(layout), c(2, 2))
})

test_that("design_to_excel_layout returns invisibly when exporting to Excel", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  tmpfile <- tempfile(fileext = ".xlsx")
  expect_invisible(
    design_to_excel_layout(df, value_column = "treatments",
                           filename = tmpfile, export_excel = TRUE)
  )
  expect_true(file.exists(tmpfile))
  unlink(tmpfile)
})

test_that("design_to_excel_layout uses custom palette", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  tmpfile <- tempfile(fileext = ".xlsx")
  expect_invisible(
    design_to_excel_layout(df, value_column = "treatments",
                           filename = tmpfile, export_excel = TRUE,
                           palette = c("#FF0000", "#00FF00"))
  )
  expect_true(file.exists(tmpfile))
  unlink(tmpfile)
})