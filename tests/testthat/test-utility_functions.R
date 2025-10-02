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
    mockery::stub(biometryassist:::.onAttach, "rlang::is_installed", function(...) FALSE)

    # Use expect_message since .onAttach uses packageStartupMessage()
    expect_message(
        biometryassist:::.onAttach(pkg = "biometryassist"),
        "biometryassist version"
    )
})


test_that("Warning prints if cran version is newer", {
    rlang::local_interactive(value = TRUE)
    local_mocked_bindings(compare_version = function(...) 1L)
    expect_warning(print(biometryassist:::.onAttach(pkg = "biometryassist")))
})

test_that("handle_deprecated_param warns when old param is provided and new param is given", {
    test_fun <- function(old = 1, new = 2) {
        handle_deprecated_param("old", "new")
        old + new
    }
    expect_warning(test_fun(old = 5, new = 2), "deprecated.*use `new` instead")
})

test_that("handle_deprecated_param warns when old param is provided and no new param", {
    test_fun <- function(old = 1) {
        handle_deprecated_param("old")
        old
    }
    expect_warning(test_fun(old = 5), "deprecated")
})

test_that("handle_deprecated_param includes custom message if provided", {
    test_fun <- function(old = 1) {
        handle_deprecated_param("old", custom_msg = "This is a custom message.")
        old
    }
    expect_warning(test_fun(old = 5), "custom message")
})

test_that("handle_deprecated_param does not warn if old param is missing", {
    test_fun <- function(old = 1) {
        handle_deprecated_param("old", "new")
        42
    }
    expect_silent(test_fun())
})

