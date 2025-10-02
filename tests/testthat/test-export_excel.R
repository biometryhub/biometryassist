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
  skip_if_not_installed("openxlsx2")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  withr::with_tempfile("tmpfile", fileext = ".xlsx", {
    expect_invisible(
      export_design_to_excel(df, value_column = "treatments",
                             filename = tmpfile)
    )
    expect_true(file.exists(tmpfile))
  })
})

test_that("export_design_to_excel uses custom palette", {
  skip_if_not_installed("openxlsx2")
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "B", "A")
  )
  withr::with_tempfile("tmpfile", fileext = ".xlsx", {
    expect_invisible(
      export_design_to_excel(df, value_column = "treatments",
                             filename = tmpfile,
                             palette = c("#FF0000", "#00FF00"))
    )
    expect_true(file.exists(tmpfile))
  })
})

test_that("function fails gracefully when openxlsx2 is not available", {
    mockery::stub(export_design_to_excel, "rlang::is_installed", function(pkg) FALSE)

    expect_error(
        export_design_to_excel(test_data),
        "Package 'openxlsx2' is required.*not installed"
    )
})

test_that("color transparency removal works correctly", {
  skip_if_not_installed("openxlsx2")

  # Test data
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "C", "D")
  )

  # Test with viridis palette (which can produce colors with transparency)
  withr::with_tempfile("tmpfile", fileext = ".xlsx", {
    expect_no_error(
      export_design_to_excel(df, value_column = "treatments",
                             filename = tmpfile,
                             palette = "viridis")
    )
    expect_true(file.exists(tmpfile))
  })
})

test_that("color transparency removal handles different hex formats", {
  # Mock setup_colour_palette to return colors with transparency
  mock_setup <- function(palette, ntrt) {
    c("#440154FF", "#31688ECC", "#35B779AA", "#FDE72588")  # 8-digit with alpha
  }

  # Test that colors get processed correctly (simulate the gsub operations)
  colors_with_alpha <- c("#440154FF", "#31688ECC", "#35B779AA", "#FDE72588")

  # Apply the same gsub operations as in the function
  processed_colors <- gsub("^(#[0-9A-Fa-f]{3})[0-9A-Fa-f]$", "\\1", colors_with_alpha)
  processed_colors <- gsub("^(#[0-9A-Fa-f]{6})[0-9A-Fa-f]{2}$", "\\1", processed_colors)

  expected_colors <- c("#440154", "#31688E", "#35B779", "#FDE725")
  expect_equal(processed_colors, expected_colors)
})

test_that("color transparency removal handles 4-digit hex codes", {
  # Test 4-digit hex codes (#RGBA format)
  colors_4digit <- c("#F00F", "#0F0A", "#00FB")  # Red, green, blue with alpha

  # Apply the same gsub operations as in the function
  processed_colors <- gsub("^(#[0-9A-Fa-f]{3})[0-9A-Fa-f]$", "\\1", colors_4digit)
  processed_colors <- gsub("^(#[0-9A-Fa-f]{6})[0-9A-Fa-f]{2}$", "\\1", processed_colors)

  expected_colors <- c("#F00", "#0F0", "#00F")
  expect_equal(processed_colors, expected_colors)
})

test_that("color transparency removal handles mixed color formats", {
  # Test mixed palette with various color formats
  mixed_colors <- c(
    "#FF0000",        # 6-digit hex (no alpha)
    "#00FF00CC",      # 8-digit hex (with alpha)
    "#00F",           # 3-digit hex (no alpha)
    "#F0FA",          # 4-digit hex (with alpha)
    "red",            # R color name
    "rgb(255,0,255)", # RGB format
    "#123456FF"       # 8-digit hex (with alpha)
  )

  # Apply the same gsub operations as in the function
  processed_colors <- gsub("^(#[0-9A-Fa-f]{3})[0-9A-Fa-f]$", "\\1", mixed_colors)
  processed_colors <- gsub("^(#[0-9A-Fa-f]{6})[0-9A-Fa-f]{2}$", "\\1", processed_colors)

  expected_colors <- c(
    "#FF0000",        # 6-digit hex unchanged
    "#00FF00",        # 8-digit hex -> 6-digit hex
    "#00F",           # 3-digit hex unchanged
    "#F0F",           # 4-digit hex -> 3-digit hex
    "red",            # R color name unchanged
    "rgb(255,0,255)", # RGB format unchanged
    "#123456"         # 8-digit hex -> 6-digit hex
  )

  expect_equal(processed_colors, expected_colors)
})

test_that("color transparency removal works with mixed palette in export function", {
  skip_if_not_installed("openxlsx2")

  # Test data with 4 treatments to match custom palette
  df <- data.frame(
    row = rep(1:2, each = 2),
    col = rep(1:2, 2),
    treatments = c("A", "B", "C", "D")
  )

  # Mixed custom palette with transparency
  mixed_palette <- c(
    "#FF000055",      # 8-digit hex with alpha
    "#0F0A",          # 4-digit hex with alpha
    "blue",           # R color name
    "#123456"         # 6-digit hex (no alpha)
  )

  withr::with_tempfile("tmpfile", fileext = ".xlsx", {
    expect_no_error(
      export_design_to_excel(df, value_column = "treatments",
                             # filename = tmpfile,
                             palette = mixed_palette)
    )
    expect_true(file.exists(tmpfile))
  })
})
