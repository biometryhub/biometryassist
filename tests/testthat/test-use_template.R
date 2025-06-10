test_that("list_templates returns character vector and finds known templates", {
  templates <- list_templates()
  expect_type(templates, "character")
  # If package is installed correctly, should find at least one template
  expect_true(any(grepl("mixed_model_template\\.R", templates)))
})

test_that("use_template copies default template to working directory", {
  skip_on_cran()
  withr::with_tempdir({
    dest <- use_template(open = FALSE)
    expect_true(file.exists("analysis_script.R"))
    expect_equal(normalizePath(dest), normalizePath("analysis_script.R"))
  })
})

test_that("use_template copies and renames template with output_name", {
  skip_on_cran()
  withr::with_tempdir({
    dest <- use_template(output_name = "custom_script.R", open = FALSE)
    expect_true(file.exists("custom_script.R"))
    expect_equal(normalizePath(dest), normalizePath("custom_script.R"))
  })
})

test_that("use_template copies user-supplied template file", {
  skip_on_cran()
  withr::with_tempdir({
    # Create a fake template file
    writeLines("my custom template", "my_template.R")
    dest <- use_template(template_name = "my_template.R", open = FALSE)
    expect_true(file.exists("analysis_script.R"))
    expect_equal(readLines("analysis_script.R"), "my custom template")
  })
})

test_that("use_template warns and falls back to default if template file does not exist", {
  skip_on_cran()
  withr::with_tempdir({
    expect_warning(
      dest <- use_template(template_name = "not_a_real_template.R", open = FALSE),
      "not found as a file"
    )
    expect_true(file.exists("analysis_script.R"))
  })
})

test_that("use_template creates destination directory if needed", {
  skip_on_cran()
  withr::with_tempdir({
    expect_false(dir.exists("newdir"))
    dest <- use_template(dest_dir = "newdir", open = FALSE)
    expect_true(dir.exists("newdir"))
    expect_true(file.exists(file.path("newdir", "analysis_script.R")))
  })
})

test_that("use_template does not overwrite existing file unless overwrite = TRUE", {
  skip_on_cran()
  withr::with_tempdir({
    writeLines("existing", "analysis_script.R")
    expect_message(
      dest <- use_template(open = FALSE, overwrite = FALSE),
      "already exists"
    )
    expect_equal(readLines("analysis_script.R"), "existing")
    # Now overwrite
    dest2 <- use_template(open = FALSE, overwrite = TRUE)
    expect_true(file.exists("analysis_script.R"))
    expect_false(any(readLines("analysis_script.R") == "existing"))
  })
})

test_that("use_template opens file in editor if open = TRUE", {
  skip_on_cran()
  withr::with_tempdir({
    # Remove 'opened' if it exists from previous tests
    if (exists("opened", envir = .GlobalEnv)) rm(opened, envir = .GlobalEnv)
    # Mock file.edit in the correct environment
    mockery::stub(use_template, "utils::file.edit", function(path) { assign("opened", path, envir = .GlobalEnv); TRUE })
    use_template(open = TRUE)
    expect_true(exists("opened", envir = .GlobalEnv))
    expect_equal(normalizePath(get("opened", envir = .GlobalEnv)), normalizePath("analysis_script.R"))
    rm(opened, envir = .GlobalEnv)
  })
})

test_that("use_template input validation works", {
  expect_error(use_template(template_name = c("a", "b")), "must be a single character string")
  expect_error(use_template(dest_dir = c("a", "b")), "must be a single character string")
  expect_error(use_template(open = c(TRUE, FALSE)), "must be TRUE or FALSE")
  expect_error(use_template(overwrite = c(TRUE, FALSE)), "must be TRUE or FALSE")
  expect_error(use_template(output_name = c("a", "b")), "must be a single character string")
})
