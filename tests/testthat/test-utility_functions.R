test_that("quiet supresses output", {
    expect_silent(quiet(print("Hello")))
    expect_silent(quiet(cat("Hello")))
})

test_that("Package message prints on load", {
    rlang::local_interactive(value = TRUE)
    expect_snapshot(biometryassist:::.onAttach(pkg = "biometryassist"))
})

test_that("Output prints if crayon is not installed", {
    rlang::local_interactive(value = TRUE)
    local_mocked_bindings(.check_package_available = function(...) FALSE)
    expect_output(print(biometryassist:::.onAttach(pkg = "biometryassist")))
})

test_that("Warning prints if cran version is newer", {
    rlang::local_interactive(value = TRUE)
    local_mocked_bindings(.compare_version = function(...) 1L)
    expect_warning(print(biometryassist:::.onAttach(pkg = "biometryassist")))
})
