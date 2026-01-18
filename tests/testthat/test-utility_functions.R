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
    local_mocked_bindings(.compare_version = function(...) 1L)
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

test_that("n_unique counts unique values in numeric vectors", {
    expect_equal(n_unique(c(1, 2, 3, 4, 5)), 5)
    expect_equal(n_unique(c(1, 1, 2, 2, 3)), 3)
    expect_equal(n_unique(c(1, 1, 1, 1, 1)), 1)
    expect_equal(n_unique(c(1.5, 2.5, 1.5, 3.5)), 3)
})

test_that("n_unique counts unique values in character vectors", {
    expect_equal(n_unique(c("a", "b", "c")), 3)
    expect_equal(n_unique(c("a", "a", "b", "b")), 2)
    expect_equal(n_unique(c("apple", "banana", "cherry", "apple")), 3)
})

test_that("n_unique counts unique values in factor vectors", {
    expect_equal(n_unique(factor(c("low", "medium", "high"))), 3)
    expect_equal(n_unique(factor(c("A", "B", "A", "C", "B"))), 3)
})

test_that("n_unique handles NA values with na.rm = FALSE (default)", {
    expect_equal(n_unique(c(1, 2, NA, 3)), 4)
    expect_equal(n_unique(c(1, NA, NA, 2)), 3)
    expect_equal(n_unique(c("a", "b", NA, "c")), 4)
})

test_that("n_unique handles NA values with na.rm = TRUE", {
    expect_equal(n_unique(c(1, 2, NA, 3), na.rm = TRUE), 3)
    expect_equal(n_unique(c(1, NA, NA, 2), na.rm = TRUE), 2)
    expect_equal(n_unique(c("a", "b", NA, "c"), na.rm = TRUE), 3)
    expect_equal(n_unique(c(1, 1, NA, NA, 2), na.rm = TRUE), 2)
})

test_that("n_unique handles edge cases", {
    expect_equal(n_unique(integer(0)), 0)
    expect_equal(n_unique(character(0)), 0)
    expect_equal(n_unique(c(1)), 1)
    expect_equal(n_unique(c(NA)), 1)
    expect_equal(n_unique(c(NA, NA, NA)), 1)
    expect_equal(n_unique(c(NA, NA, NA), na.rm = TRUE), 0)
})
