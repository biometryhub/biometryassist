# R/test-prediction_methods.R

test_that("get_predictions.asreml calls predict.asreml when pred.obj is missing or NULL", {
    skip_if_not_installed("mockery")

    # Create a mock predict.asreml that returns a simple prediction object
    predict_result <- list(
        pvals = data.frame(
            predicted.value = c(1, 2, 3),
            std.error = c(0.1, 0.2, 0.3),
            status = c("OK", "OK", "OK"),
            Treatment = c("A", "B", "C")
        ),
        sed = matrix(0.1, 3, 3)
    )

    # Create a mock for asreml::wald
    wald_result <- list(
        Wald = structure(
            data.frame(denDF = c(10, 10, 10)),
            row.names = c("Treatment", "B", "C")
        )
    )

    # Create a minimal mock model.obj
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ 1")
        ),
        nedf = 99
    ), class = "asreml")

    # Mock the required functions
    mockery::stub(get_predictions.asreml, "asreml::predict.asreml", function(...) predict_result)
    mockery::stub(get_predictions.asreml, "asreml::wald", function(...) wald_result)
    mockery::stub(get_predictions.asreml, "quiet", function(x) x)

    # Test with pred.obj missing
    result <- get_predictions.asreml(model_obj, classify = "Treatment")

    # Check the result structure
    expect_type(result, "list")
    expect_true(all(c("predictions", "sed", "df", "ylab", "aliased_names") %in% names(result)))
    expect_equal(result$predictions$predicted.value, c(1, 2, 3))
    expect_equal(result$df, 10)

    # Test with pred.obj = NULL explicitly
    result2 <- get_predictions.asreml(model_obj, classify = "Treatment", pred.obj = NULL)

    # Should get same results
    expect_equal(result2$predictions$predicted.value, c(1, 2, 3))
})

test_that("get_predictions.asreml errors if all predicted values and std.errors are NA", {
    skip_if_not_installed("mockery")

    # Create mock prediction object with all NA values
    pred_obj <- list(
        pvals = data.frame(
            predicted.value = c(NA, NA),
            std.error = c(NA, NA),
            status = c("aliased", "aliased"),
            Treatment = c("A", "B")
        ),
        sed = matrix(NA, 2, 2)
    )

    # Create a minimal mock model.obj
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ 1")
        ),
        nedf = 99
    ), class = "asreml")

    # Test that error is thrown with the expected message
    expect_error(
        get_predictions.asreml(model_obj, classify = "Treatment", pred.obj = pred_obj),
        "All predicted values are aliased. Perhaps you need the `present` argument?",
        fixed = TRUE
    )
})

test_that("get_predictions.asreml errors if classify is not in model terms", {
    skip_if_not_installed("mockery")

    # Create a minimal mock model.obj
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ Block")
        ),
        nedf = 99
    ), class = "asreml")

    # Test that error is thrown with the expected message
    expect_error(
        get_predictions.asreml(model_obj, classify = "InvalidTerm"),
        "InvalidTerm is not a term in the model. Please check model specification.",
        fixed = TRUE
    )
})

test_that("get_predictions.asreml handles partially aliased treatments correctly", {
    skip_if_not_installed("mockery")

    # Create a mock predict.asreml that returns predictions with one aliased value
    predict_result <- list(
        pvals = data.frame(
            predicted.value = c(1, NA, 3),
            std.error = c(0.1, NA, 0.3),
            status = c("OK", "aliased", "OK"),
            Treatment = c("A", "B", "C")
        ),
        sed = matrix(c(0.1, 0.1, 0.1, 0.1, NA, 0.1, 0.1, 0.1, 0.1), 3, 3)
    )

    # Create a mock for asreml::wald
    wald_result <- list(
        Wald = structure(
            data.frame(denDF = c(10, 10, 10)),
            row.names = c("Treatment", "B", "C")
        )
    )

    # Create a minimal mock model.obj
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ 1")
        ),
        nedf = 99
    ), class = "asreml")

    # Mock the required functions
    mockery::stub(get_predictions.asreml, "asreml::predict.asreml", function(...) predict_result)
    mockery::stub(get_predictions.asreml, "asreml::wald", function(...) wald_result)
    mockery::stub(get_predictions.asreml, "quiet", function(x) x)

    # Expect a warning about the aliased treatment
    expect_warning(
        result <- get_predictions.asreml(model_obj, classify = "Treatment"),
        "A level of Treatment is aliased"
    )

    # Check the aliased treatment was removed and recorded
    expect_equal(nrow(result$predictions), 2)
    expect_equal(result$aliased_names, "B")
    expect_equal(dim(result$sed), c(2, 2))
})

test_that("get_predictions.asreml uses model.obj$nedf when term not in fixed effects", {
    skip_if_not_installed("mockery")

    # Create mock results
    predict_result <- list(
        pvals = data.frame(
            predicted.value = c(1, 2, 3),
            std.error = c(0.1, 0.2, 0.3),
            status = c("OK", "OK", "OK"),
            Block = c("1", "2", "3")
        ),
        sed = matrix(0.1, 3, 3)
    )

    # Wald table without the Block term
    wald_result <- list(
        Wald = structure(
            data.frame(denDF = c(10, 10)),
            row.names = c("Treatment", "Intercept")
        )
    )

    # Create model with Block in random terms
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ Block")
        ),
        nedf = 99
    ), class = "asreml")

    # Mock the required functions
    mockery::stub(get_predictions.asreml, "asreml::predict.asreml", function(...) predict_result)
    mockery::stub(get_predictions.asreml, "asreml::wald", function(...) wald_result)
    mockery::stub(get_predictions.asreml, "quiet", function(x) x)

    # Expect a warning about using residual df
    expect_warning(
        result <- get_predictions.asreml(model_obj, classify = "Block"),
        "Block is not a fixed term in the model"
    )

    # Check that nedf was used
    expect_equal(result$df, 99)
})

test_that("get_predictions.asreml returns correct ylab from model formula", {
    skip_if_not_installed("mockery")

    # Create mock results
    predict_result <- list(
        pvals = data.frame(
            predicted.value = c(1, 2, 3),
            std.error = c(0.1, 0.2, 0.3),
            status = c("OK", "OK", "OK"),
            Treatment = c("A", "B", "C")
        ),
        sed = matrix(0.1, 3, 3)
    )

    wald_result <- list(
        Wald = structure(
            data.frame(denDF = c(10, 10, 10)),
            row.names = c("Treatment", "B", "C")
        )
    )

    # Create model with a specific response variable
    model_obj <- structure(list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~ 1")
        ),
        nedf = 99
    ), class = "asreml")

    # Mock the required functions
    mockery::stub(get_predictions.asreml, "asreml::predict.asreml", function(...) predict_result)
    mockery::stub(get_predictions.asreml, "asreml::wald", function(...) wald_result)
    mockery::stub(get_predictions.asreml, "quiet", function(x) x)

    result <- get_predictions.asreml(model_obj, classify = "Treatment")

    # Check that ylab is correctly extracted
    expect_equal(result$ylab, quote(yield))
})

# Add a helper function to make the tests work without requiring the actual %!in% operator
`%!in%` <- function(x, y) !(x %in% y)
