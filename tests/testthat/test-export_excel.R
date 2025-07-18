test_that("export_design_to_excel returns correct layout matrix", {
  df <- data.frame(
    row = rep(1:2, each = 3),
    col = rep(1:3, 2),
    treatments = c("A", "B", "C", "C", "B", "A")
  )
  layout <- export_design_to_excel(df, value_column = "treatments")
  expect_s3_class(layout, "data.frame")
  expect_equal(dim(layout), c(2, 3))
  expect_equal(layout[1, 1], "A")
  expect_equal(layout[2, 3], "A")
})

test_that("export_design_to_excel errors if required columns are missing", {
  df <- data.frame(row = 1:2, treatments = c("A", "B"))
  expect_error(export_design_to_excel(df, value_column = "treatments"),
               "Missing required columns")
})

test_that("export_design_to_excel works with a list input", {
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  design_list <- list(design = df)
  layout <- export_design_to_excel(design_list, value_column = "treatments")
  expect_s3_class(layout, "data.frame")
  expect_equal(dim(layout), c(2, 2))
})

test_that("export_design_to_excel returns invisibly when exporting to Excel", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  tmpfile <- tempfile(fileext = ".xlsx")
  expect_invisible(
    export_design_to_excel(df, value_column = "treatments",
                           filename = tmpfile)
  )
  expect_true(file.exists(tmpfile))
  unlink(tmpfile)
})

test_that("export_design_to_excel uses custom palette", {
  skip_if_not_installed("openxlsx")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  tmpfile <- tempfile(fileext = ".xlsx")
  expect_invisible(
    export_design_to_excel(df, value_column = "treatments",
                           filename = tmpfile,
                           palette = c("#FF0000", "#00FF00"))
  )
  expect_true(file.exists(tmpfile))
  unlink(tmpfile)
})

test_that("function fails gracefully when openxlsx is not available", {
    mockery::stub(export_design_to_excel, "rlang::is_installed", function(pkg) FALSE)
    
    expect_error(
        export_design_to_excel(test_data),
        "Package 'openxlsx' is required.*not installed"
    )
})
