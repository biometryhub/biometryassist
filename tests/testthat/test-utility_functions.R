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
    
    # Mock to simulate crayon not being installed
    mockery::stub(biometryassist:::.onAttach, "rlang::is_installed", function(pkg) {
        if (pkg == "crayon") return(FALSE)
        TRUE
    })
    
    # Mock compare_version to avoid network calls and avoid triggering update warning
    mockery::stub(biometryassist:::.onAttach, "compare_version", function(...) 0L)

    # Use expect_message since .onAttach uses packageStartupMessage()
    # This should hit the else branch (line 45-47) which calls packageStartupMessage without crayon
    expect_message(
        biometryassist:::.onAttach(pkg = "biometryassist"),
        "biometryassist version"
    )
})

test_that("Output prints with crayon when it is installed", {
    skip_if_not_installed("crayon")
    rlang::local_interactive(value = TRUE)
    
    # Simply call .onAttach when crayon IS installed (default behavior)
    # This will naturally test the if(rlang::is_installed("crayon")) branch
    # Mock compare_version to avoid network calls
    local_mocked_bindings(
        compare_version = function(...) 0L,
        .package = "biometryassist"
    )
    
    # Should use crayon::green (lines 42-43) since crayon is installed
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

# n_unique() tests ----

test_that("n_unique works with basic numeric vectors", {
    expect_equal(n_unique(c(1, 2, 3, 4, 5)), 5)
    expect_equal(n_unique(c(1, 1, 2, 2, 3)), 3)
    expect_equal(n_unique(c(1)), 1)
})

test_that("n_unique works with character vectors", {
    expect_equal(n_unique(c("a", "b", "c")), 3)
    expect_equal(n_unique(c("a", "a", "b", "b")), 2)
    expect_equal(n_unique(c("hello")), 1)
})

test_that("n_unique works with factor vectors", {
    expect_equal(n_unique(factor(c("low", "med", "high"))), 3)
    expect_equal(n_unique(factor(c("A", "A", "B", "B", "C"))), 3)
    expect_equal(n_unique(factor(c("single"))), 1)
})

test_that("n_unique handles empty vectors", {
    expect_equal(n_unique(numeric(0)), 0)
    expect_equal(n_unique(character(0)), 0)
    expect_equal(n_unique(factor(character(0))), 0)
})

test_that("n_unique handles all NA values", {
    expect_equal(n_unique(c(NA, NA, NA)), 1)
    expect_equal(n_unique(c(NA_real_, NA_real_)), 1)
    expect_equal(n_unique(c(NA_character_, NA_character_)), 1)
})

test_that("n_unique handles all NA values with na.rm=TRUE", {
    expect_equal(n_unique(c(NA, NA, NA), na.rm = TRUE), 0)
    expect_equal(n_unique(c(NA_real_, NA_real_), na.rm = TRUE), 0)
    expect_equal(n_unique(c(NA_character_, NA_character_), na.rm = TRUE), 0)
})

test_that("n_unique handles mixed NA values with na.rm=FALSE", {
    expect_equal(n_unique(c(1, 2, NA, 3, NA)), 4)
    expect_equal(n_unique(c("a", "b", NA, "c")), 4)
    expect_equal(n_unique(c(1, 1, NA, 2, NA)), 3)
})

test_that("n_unique handles mixed NA values with na.rm=TRUE", {
    expect_equal(n_unique(c(1, 2, NA, 3, NA), na.rm = TRUE), 3)
    expect_equal(n_unique(c("a", "b", NA, "c"), na.rm = TRUE), 3)
    expect_equal(n_unique(c(1, 1, NA, 2, NA), na.rm = TRUE), 2)
})

test_that("n_unique handles vectors with duplicates correctly", {
    expect_equal(n_unique(rep(1, 10)), 1)
    expect_equal(n_unique(rep("x", 5)), 1)
    expect_equal(n_unique(c(1, 2, 1, 2, 1, 2)), 2)
})

# construct_factorial_labels() tests ----

test_that("construct_factorial_labels works with default separators", {
    # Create a simple design_book with 2 factors
    design_book <- data.frame(
        plot = 1:4,
        block = 1,
        A = c("a1", "a1", "a2", "a2"),
        B = c("b1", "b2", "b1", "b2")
    )
    
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 3)
    
    # Default separators: c("", " ") means no separator between name and level, space between factors
    expect_equal(as.character(result), c("Aa1 Bb1", "Aa1 Bb2", "Aa2 Bb1", "Aa2 Bb2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels works with single custom separator", {
    # Create a simple design_book
    design_book <- data.frame(
        plot = 1:2,
        A = c("low", "high"),
        B = c("x", "y")
    )
    
    # When a single separator is provided, it's used for both positions
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2, fac.sep = "_")
    
    expect_equal(as.character(result), c("A_low_B_x", "A_high_B_y"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels works with two custom separators", {
    # Create a design_book
    design_book <- data.frame(
        id = 1:2,
        Variety = c("V1", "V2"),
        Fertilizer = c("F1", "F2")
    )
    
    # First separator between name and level, second between factors
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2, fac.sep = c(":", " x "))
    
    expect_equal(as.character(result), c("Variety:V1 x Fertilizer:F1", "Variety:V2 x Fertilizer:F2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels handles single factor column", {
    # Design book with only one factor column to combine
    design_book <- data.frame(
        plot = 1:3,
        treatment = c("T1", "T2", "T3")
    )
    
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 2)
    
    expect_equal(as.character(result), c("treatmentT1", "treatmentT2", "treatmentT3"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels handles three factors", {
    # Design book with three factors
    design_book <- data.frame(
        A = c("a1", "a2"),
        B = c("b1", "b2"),
        C = c("c1", "c2")
    )
    
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c("=", ", "))
    
    expect_equal(as.character(result), c("A=a1, B=b1, C=c1", "A=a2, B=b2, C=c2"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels trims whitespace from results", {
    # Design book that might produce extra whitespace
    design_book <- data.frame(
        X = c("val1", "val2"),
        Y = c("val3", "val4")
    )
    
    # Using space as both separators could create extra spaces
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c(" ", " "))
    
    # Check that there's no leading/trailing whitespace
    expect_equal(as.character(result), c("X val1 Y val3", "X val2 Y val4"))
    expect_true(all(!grepl("^\\s|\\s$", as.character(result))))
})

test_that("construct_factorial_labels throws error for invalid start_col", {
    design_book <- data.frame(
        A = c("a1", "a2"),
        B = c("b1", "b2")
    )
    
    # start_col greater than ncol(design_book) should error
    expect_error(
        biometryassist:::construct_factorial_labels(design_book, start_col = 5),
        "start_col must be <= ncol\\(design_book\\)"
    )
})

test_that("construct_factorial_labels handles various data types in columns", {
    # Design book with numeric and character columns
    design_book <- data.frame(
        dose = c(10, 20, 30),
        method = c("A", "B", "C"),
        temp = c(20, 25, 30)
    )
    
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c(":", "|"))
    
    expect_equal(as.character(result), c("dose:10|method:A|temp:20", "dose:20|method:B|temp:25", "dose:30|method:C|temp:30"))
    expect_s3_class(result, "factor")
})

test_that("construct_factorial_labels produces correct factor levels", {
    # Create design with repeated combinations
    design_book <- data.frame(
        A = c("a1", "a2", "a1", "a2"),
        B = c("b1", "b1", "b2", "b2")
    )
    
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1)
    
    # Should have 4 treatments
    expect_length(result, 4)
    # Should have 4 unique levels
    expect_equal(nlevels(result), 4)
    # Check the actual levels
    expect_setequal(levels(result), c("Aa1 Bb1", "Aa1 Bb2", "Aa2 Bb1", "Aa2 Bb2"))
})

test_that("construct_factorial_labels works with empty string separators", {
    design_book <- data.frame(
        Factor1 = c("X", "Y"),
        Factor2 = c("1", "2")
    )
    
    # Both separators as empty strings
    result <- biometryassist:::construct_factorial_labels(design_book, start_col = 1, fac.sep = c("", ""))
    
    expect_equal(as.character(result), c("Factor1XFactor21", "Factor1YFactor22"))
    expect_s3_class(result, "factor")
})

