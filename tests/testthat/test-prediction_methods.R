test_that("get_predictions.asreml uses provided pred.obj when supplied", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Create mock pred.obj that would be passed in
    mock_pred_obj <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2", "0.4", "0.6"),
            predicted.value = c(100, 110, 120, 130),
            std.error = c(5, 5, 5, 5),
            status = c("Estimable", "Estimable", "Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 8, 9,
                      7, NA, 8, 9,
                      8, 8, NA, 9,
                      9, 9, 9, NA), nrow = 4, ncol = 4)
    )
    
    # Mock the asreml functions to ensure they're NOT called when pred.obj is provided
    mock_predict <- mockery::mock()
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Nitrogen")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call the function with pred.obj provided
    result <- get_predictions.asreml(mock_model, classify = "Nitrogen", pred.obj = mock_pred_obj)
    
    # Verify predict.asreml was NOT called (because pred.obj was provided)
    mockery::expect_called(mock_predict, 0)
    
    # Verify the result uses the provided pred.obj
    expect_equal(result$predictions$predicted.value, c(100, 110, 120, 130))
    expect_equal(result$predictions$std.error, c(5, 5, 5, 5))
})

test_that("get_predictions.asreml generates predictions when pred.obj is NULL", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result that would be generated
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2", "0.4", "0.6"),
            predicted.value = c(100, 110, 120, 130),
            std.error = c(5, 5, 5, 5),
            status = c("Estimable", "Estimable", "Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 8, 9,
                      7, NA, 8, 9,
                      8, 8, NA, 9,
                      9, 9, 9, NA), nrow = 4, ncol = 4)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Nitrogen")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call the function without pred.obj (NULL by default)
    result <- get_predictions.asreml(mock_model, classify = "Nitrogen")
    
    # Verify predict.asreml WAS called (because pred.obj was NULL)
    mockery::expect_called(mock_predict, 1)
    
    # Verify the correct arguments were passed to predict.asreml
    call_args <- mockery::mock_args(mock_predict)[[1]]
    expect_equal(call_args$object, mock_model)
    expect_equal(call_args$classify, "Nitrogen")
    expect_equal(call_args$sed, TRUE)
    expect_equal(call_args$trace, FALSE)
    
    # Verify the result contains the generated predictions
    expect_equal(result$predictions$predicted.value, c(100, 110, 120, 130))
})

test_that("get_predictions.asreml generates predictions when pred.obj is missing", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2"),
            predicted.value = c(100, 110),
            std.error = c(5, 5),
            status = c("Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 7, NA), nrow = 2, ncol = 2)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Nitrogen")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call without specifying pred.obj at all
    result <- get_predictions.asreml(mock_model, classify = "Nitrogen")
    
    # Verify predict.asreml WAS called
    mockery::expect_called(mock_predict, 1)
    expect_equal(result$predictions$predicted.value, c(100, 110))
})

test_that("get_predictions.asreml errors when all predicted values are aliased", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen + Variety"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result with ALL NA values (all aliased)
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2", "0.4"),
            Variety = c("A", "B", "C"),
            predicted.value = c(NA, NA, NA),
            std.error = c(NA, NA, NA),
            status = c("Aliased", "Aliased", "Aliased")
        ),
        sed = matrix(c(NA, NA, NA,
                      NA, NA, NA,
                      NA, NA, NA), nrow = 3, ncol = 3)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    
    # Expect error when all values are aliased
    expect_error(
        get_predictions.asreml(mock_model, classify = "Nitrogen:Variety"),
        "All predicted values are aliased. Perhaps you need the `present` argument?"
    )
})

test_that("get_predictions.asreml errors when all std.errors are NA but predicted values exist", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result with predicted values but ALL NA std.errors
    mock_pred_result <- list(
        pvals = data.frame(
            Treatment = c("A", "B", "C"),
            predicted.value = c(NA, NA, NA),
            std.error = c(NA, NA, NA),
            status = c("Aliased", "Aliased", "Aliased")
        ),
        sed = matrix(c(NA, NA, NA,
                      NA, NA, NA,
                      NA, NA, NA), nrow = 3, ncol = 3)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    
    # Expect error when all std.errors are NA
    expect_error(
        get_predictions.asreml(mock_model, classify = "Treatment"),
        "All predicted values are aliased. Perhaps you need the `present` argument?"
    )
})

test_that("get_predictions.asreml handles partial aliasing correctly", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Treatment"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result with SOME aliased values (not all)
    mock_pred_result <- list(
        pvals = data.frame(
            Treatment = c("A", "B", "C", "D"),
            predicted.value = c(100, 110, NA, 130),
            std.error = c(5, 5, NA, 5),
            status = c("Estimable", "Estimable", "Aliased", "Estimable")
        ),
        sed = matrix(c(NA, 7, 8, 9,
                      7, NA, 8, 9,
                      8, 8, NA, 9,
                      9, 9, 9, NA), nrow = 4, ncol = 4)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Treatment")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Should NOT error with partial aliasing (only error when ALL are aliased)
    expect_warning(
        result <- get_predictions.asreml(mock_model, classify = "Treatment"),
        "A level of Treatment is aliased"
    )
    
    # Result should only contain non-aliased values
    expect_equal(nrow(result$predictions), 3)
    expect_equal(result$predictions$predicted.value, c(100, 110, 130))
    expect_false("C" %in% result$predictions$Treatment)
})

test_that("get_predictions.asreml passes additional arguments to predict.asreml", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2"),
            predicted.value = c(100, 110),
            std.error = c(5, 5),
            status = c("Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 7, NA), nrow = 2, ncol = 2)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Nitrogen")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call with additional arguments
    result <- get_predictions.asreml(mock_model, classify = "Nitrogen", 
                                    present = c("Nitrogen", "Blocks"),
                                    aliasing.scheme = TRUE)
    
    # Verify additional arguments were passed through
    call_args <- mockery::mock_args(mock_predict)[[1]]
    expect_equal(call_args$present, c("Nitrogen", "Blocks"))
    expect_equal(call_args$aliasing.scheme, TRUE)
})

test_that("get_predictions.asreml uses provided dendf when supplied in args", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2", "0.4"),
            predicted.value = c(100, 110, 120),
            std.error = c(5, 5, 5),
            status = c("Estimable", "Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 8,
                      7, NA, 8,
                      8, 8, NA), nrow = 3, ncol = 3)
    )
    
    # Create custom dendf data frame
    custom_dendf <- data.frame(
        Source = c("Nitrogen", "Blocks"),
        denDF = c(25, 5)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock()  # Should NOT be called when dendf is provided
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call with dendf provided in args
    result <- get_predictions.asreml(mock_model, classify = "Nitrogen", dendf = custom_dendf)
    
    # Verify wald was NOT called (because dendf was provided)
    mockery::expect_called(mock_wald, 0)
    
    # Verify the custom dendf was used (should be 25, not the default from wald)
    expect_equal(result$df, 25)
})

test_that("get_predictions.asreml uses residual df when classify not found in wald output", {
    skip_if_not_installed("mockery")
    
    # Create mock model object with random term
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Variety"),
            random = as.formula("~Blocks")
        ),
        nedf = 15
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result
    mock_pred_result <- list(
        pvals = data.frame(
            Blocks = c("B1", "B2", "B3"),
            predicted.value = c(100, 105, 110),
            std.error = c(5, 5, 5),
            status = c("Estimable", "Estimable", "Estimable")
        ),
        sed = matrix(c(NA, 7, 8,
                      7, NA, 8,
                      8, 8, NA), nrow = 3, ncol = 3)
    )
    
    # Mock wald output that doesn't include Blocks (since it's a random term)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Variety")  # Only fixed term, not Blocks
    )))
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Call with Blocks (a random term not in wald output)
    expect_warning(
        result <- get_predictions.asreml(mock_model, classify = "Blocks"),
        "Blocks is not a fixed term in the model. The denominator degrees of freedom are estimated using the residual degrees of freedom. This may be inaccurate."
    )
    
    # Verify it falls back to model.obj$nedf (residual df)
    expect_equal(result$df, 15)
})

test_that("get_predictions.lmerModLmerTest delegates to lmerMod method", {
    skip_if_not_installed("mockery")
    
    # Create a mock lmerModLmerTest object
    mock_model <- structure(
        list(
            frame = data.frame(
                yield = c(100, 110, 120, 105, 115, 125),
                Treatment = factor(rep(c("A", "B"), each = 3))
            )
        ),
        class = c("lmerModLmerTest", "lmerMod")
    )
    
    # Create a mock result that would be returned by get_predictions.lm
    mock_result <- list(
        predictions = data.frame(
            Treatment = c("A", "B"),
            predicted.value = c(105, 120),
            std.error = c(5, 5),
            df = c(4, 4)
        ),
        sed = matrix(c(NA, 7, 7, NA), nrow = 2, ncol = 2),
        df = 4,
        ylab = "yield",
        aliased_names = NULL
    )
    
    # Mock get_predictions.lmerMod to return the mock result
    mock_lmerMod_method <- mockery::mock(mock_result)
    
    # Stub the lmerMod method
    mockery::stub(get_predictions.lmerModLmerTest, 'get_predictions.lmerMod', mock_lmerMod_method)
    
    # Call get_predictions.lmerModLmerTest
    result <- get_predictions.lmerModLmerTest(mock_model, classify = "Treatment")
    
    # Verify that get_predictions.lmerMod was called with correct arguments
    mockery::expect_called(mock_lmerMod_method, 1)
    call_args <- mockery::mock_args(mock_lmerMod_method)[[1]]
    expect_equal(call_args[[1]], mock_model)
    expect_equal(call_args[[2]], "Treatment")
    
    # Verify the result is what was returned by the lmerMod method
    expect_equal(result$predictions$predicted.value, c(105, 120))
    expect_equal(result$df, 4)
    expect_equal(result$ylab, "yield")
})

test_that("get_predictions.asreml handles multiple aliased values with plural warning", {
    skip_if_not_installed("mockery")
    
    # Create mock model object
    mock_model <- list(
        formulae = list(
            fixed = as.formula("yield ~ Nitrogen + Variety"),
            random = as.formula("~Blocks")
        ),
        nedf = 10
    )
    class(mock_model) <- "asreml"
    
    # Mock prediction result with MULTIPLE aliased values (not all)
    mock_pred_result <- list(
        pvals = data.frame(
            Nitrogen = c("0", "0.2", "0.4", "0.6", "0.8"),
            Variety = c("A", "B", "C", "D", "E"),
            predicted.value = c(100, 110, NA, 130, NA),
            std.error = c(5, 5, NA, 5, NA),
            status = c("Estimable", "Estimable", "Aliased", "Estimable", "Aliased")
        ),
        sed = matrix(c(NA, 7, 8, 9, 10,
                      7, NA, 8, 9, 10,
                      8, 8, NA, 9, 10,
                      9, 9, 9, NA, 10,
                      10, 10, 10, 10, NA), nrow = 5, ncol = 5)
    )
    
    # Mock the asreml functions
    mock_predict <- mockery::mock(mock_pred_result)
    mock_wald <- mockery::mock(list(Wald = data.frame(
        denDF = c(10),
        row.names = c("Nitrogen:Variety")
    )))
    
    mockery::stub(get_predictions.asreml, 'asreml::predict.asreml', mock_predict)
    mockery::stub(get_predictions.asreml, 'asreml::wald', mock_wald)
    
    # Expect warning with plural message for multiple aliased values
    expect_warning(
        result <- get_predictions.asreml(mock_model, classify = "Nitrogen:Variety"),
        "Some levels of Nitrogen:Variety are aliased. They have been removed from predicted output.
  Aliased levels are: 0\\.4:C, 0\\.8:E.
  These levels are saved in the output object."
    )
    
    # Result should only contain non-aliased values
    expect_equal(nrow(result$predictions), 3)
    expect_equal(result$predictions$predicted.value, c(100, 110, 130))
    expect_true(all(c("0.4:C", "0.8:E") %in% result$aliased_names))
    expect_equal(length(result$aliased_names), 2)
})
