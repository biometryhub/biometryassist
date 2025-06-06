load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)

test_that("function works", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))
    oats.logl <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                           resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 5, quiet = TRUE)
    oats.logl2 <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                            resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 5, numeric = TRUE, quiet = TRUE)
    oats.logl3 <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                            resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 1, quiet = TRUE)

    expect_equal(oats.logl$Term, c("Blocks", "Blocks:Wplots", "ar1(Row)", "ar1(Column)"))
    expect_equal(oats.logl$LogLRT.pvalue, c("0.11116", "0.13142", "0.00559", "0.82883"))
    expect_equal(oats.logl2$LogLRT.pvalue, c(0.11116, 0.13142, 0.00559, 0.82883))
    expect_true(is.numeric(oats.logl2$LogLRT.pvalue))
    expect_equal(oats.logl3$LogLRT.pvalue, c('0.1', '0.1', '<0.1', '0.8'))
})

test_that("logltest gives an error on different model type", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
    expect_error(logl_test(dat.aov), "Only asreml models are supported\\.")
})

test_that("logltest gives an error on different model type", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))
    expect_error(logl_test(model.asr),
                 "At least one of rand\\.terms or resid\\.terms must be provided\\.")
})


test_that("logl_test works with random terms only", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))

    expect_silent({
        result <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"))
    })

    expect_s3_class(result, "data.frame")
    expect_named(result, c("Term", "LogLRT.pvalue"))
    expect_true(all(c("Blocks", "Blocks:Wplots") %in% result$Term))
})

test_that("logl_test works with residual terms only", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))

    expect_silent({
        result <- logl_test(model.obj = model.asr, resid.terms = c("ar1(Row)", "ar1(Column)"))
    })

    expect_s3_class(result, "data.frame")
    expect_true(all(c("ar1(Row)", "ar1(Column)") %in% result$Term))
})

test_that("logl_test works with both random and residual terms", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))

    expect_silent({
        result <- logl_test(
            model.obj = model.asr,
            rand.terms = c("Blocks", "Blocks:Wplots"),
            resid.terms = c("ar1(Row)", "ar1(Column)")
        )
    })

    expect_s3_class(result, "data.frame")
    expect_true(all(c("Blocks", "Blocks:Wplots", "ar1(Row)", "ar1(Column)") %in% result$Term))
})

test_that("logl_test returns numeric p-values when numeric = TRUE", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))

    result <- logl_test(model.obj = model.asr, rand.terms = c("Blocks"), numeric = TRUE)
    expect_type(result$LogLRT.pvalue, "double")
})

# test_that("logl_test handles small p-values with numeric = FALSE", {
#     skip_if_not(.check_package_available("asreml"))
#     quiet(library(asreml))
#
#     result <- logl_test(model.obj = model.asr, resid.terms = c("ar1(Row)"), decimals = 2, numeric = FALSE)
#     expect_type(result$LogLRT.pvalue, "character")
#     expect_true(any(grepl("^<", result$LogLRT.pvalue)))
# })

test_that("logl_test throws error for non-asreml model", {
    skip_if_not(.check_package_available("asreml"))
    expect_error(logl_test(model.obj = lm(Sepal.Length ~ Species, data = iris), rand.terms = "Species"),
                 "Only asreml models are supported")
})

test_that("logl_test throws error when both term lists are NULL", {
    skip_if_not(.check_package_available("asreml"))
    quiet(library(asreml))

    expect_error(logl_test(model.obj = model.asr),
                 "At least one of rand\\.terms or resid\\.terms must be provided\\.")
})

library(testthat)
library(mockery)

# Mock ASReml model object creator
create_mock_asreml_model <- function(converged = TRUE, loglik = -100,
                                     vparameters = c(1.5, 0.8),
                                     fixed_coefs = c(10, 5, 2),
                                     varcomp_boundary = character(0),
                                     random_formula = ~ Block + Block:Plot,
                                     residual_formula = ~ ar1(Row):ar1(Col)) {

    # Create variance components summary
    varcomp_names <- c("Block", "Block:Plot", "ar1(Row)", "ar1(Col)", "units")
    varcomp_df <- data.frame(
        component = c(1.2, 0.8, 0.6, 0.4, 1.0),
        std.error = c(0.2, 0.1, 0.1, 0.1, 0.1),
        z.ratio = c(6, 8, 6, 4, 10),
        bound = c(rep("P", 5)),
        stringsAsFactors = FALSE
    )
    rownames(varcomp_df) <- varcomp_names

    # Set boundary terms
    if (length(varcomp_boundary) > 0) {
        varcomp_df[varcomp_boundary, "bound"] <- "B"
    }

    model <- list(
        converge = converged,
        loglik = loglik,
        vparameters = vparameters,
        coefficients = list(fixed = fixed_coefs),
        formulae = list(
            random = random_formula,
            residual = residual_formula
        )
    )

    class(model) <- "asreml"
    attr(model, "summary_varcomp") <- varcomp_df

    return(model)
}

test_that("logl_test validates input correctly", {
    # Test non-asreml object
    expect_error(
        logl_test(list(class = "lm")),
        "Only asreml models are supported"
    )

    # Test missing terms
    mock_model <- create_mock_asreml_model()
    expect_error(
        logl_test(mock_model),
        "At least one of rand.terms or resid.terms must be provided"
    )
})

test_that("logl_test handles boundary terms correctly", {
    # Create model with boundary terms
    mock_model <- create_mock_asreml_model(varcomp_boundary = c("Block", "ar1(Row)"))

    # Mock summary function
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))

    # Mock asreml update function
    mock_update <- mock(mock_model, cycle = TRUE)

    # Mock quiet function
    mock_quiet_fn <- mock(mock_model, cycle = TRUE)

    # Mock format.pval
    mock_format_pval <- mock("< 0.001", "0.023", "1.000", "0.156", cycle = TRUE)

    # Use mockery to mockery::stub the functions
    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model,
                        rand.terms = c("Block", "Block:Plot"),
                        resid.terms = c("ar1(Row)", "ar1(Col)"))

    # Check that boundary terms have p-value = 1
    expect_false("Block" %in% result$Term)
    expect_true("ar1(Row)" %in% result$Term)

    # Check structure
    expect_true(all(c("Term", "LogLRT.pvalue") %in% colnames(result)))
    expect_equal(nrow(result), 3)
})

test_that("logl_test processes random terms correctly", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model for testing
    reduced_model <- mock_model
    reduced_model$loglik <- -102  # Slightly worse fit
    reduced_model$vparameters <- c(0.8)  # One less parameter

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(reduced_model, cycle = TRUE)
    mock_format_pval <- mock("0.045", "0.123", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model, rand.terms = c("Block", "Block:Plot"))

    # Check result structure
    expect_equal(nrow(result), 2)
    expect_equal(result$Term, c("Block", "Block:Plot"))
    expect_true(all(c("Term", "LogLRT.pvalue") %in% colnames(result)))

    # Verify mocks were called
    expect_called(mock_summary, 1)
    expect_called(mock_update, 2)  # Called for each term
})

test_that("logl_test handles AR residual terms correctly", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -101.5
    reduced_model$vparameters <- c(1.5)

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(reduced_model, cycle = TRUE)
    mock_format_pval <- mock("0.067", "0.234", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model, resid.terms = c("ar1(Row)", "ar1(Col)"))

    # Check result structure
    expect_equal(nrow(result), 2)
    expect_equal(result$Term, c("ar1(Row)", "ar1(Col)"))

    # Verify update was called with residual modifications
    expect_called(mock_update, 2)
})

test_that("logl_test handles non-AR residual terms correctly", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -101.8

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(reduced_model, cycle = TRUE)
    mock_format_pval <- mock("0.089", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model, resid.terms = c("exp(Row)"))

    # Check result structure
    expect_equal(nrow(result), 1)
    expect_equal(result$Term, "exp(Row)")
})

test_that("logl_test handles mixed random and residual terms", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -103
    reduced_model$vparameters <- c(0.8)

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(reduced_model, cycle = TRUE)
    mock_format_pval <- mock("0.034", "0.167", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model,
                        rand.terms = c("Block"),
                        resid.terms = c("ar1(Row)"))

    # Check that both types are included
    expect_equal(nrow(result), 2)
    expect_true("Block" %in% result$Term)
    expect_true("ar1(Row)" %in% result$Term)
})

# test_that("logl_test respects numeric parameter", {
#     mock_model <- create_mock_asreml_model()
#
#     # Create reduced model
#     reduced_model <- mock_model
#     reduced_model$loglik <- -101
#
#     # Mock functions for numeric = TRUE
#     mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")), cycle = TRUE)
#     mock_update <- mock(reduced_model, cycle = TRUE)
#     mock_quiet_fn <- mock(reduced_model)
#
#     mockery::stub(logl_test, 'summary', mock_summary)
#     mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
#     mockery::stub(logl_test, 'quiet', mock_quiet_fn)
#
#     # Test with numeric = TRUE (no format.pval called)
#     result_numeric <- logl_test(mock_model,
#                                 rand.terms = c("Block"),
#                                 numeric = TRUE)
#     expect_true(is.numeric(result_numeric$LogLRT.pvalue))
#
#     # Test with numeric = FALSE
#     mock_format_pval <- mock("0.045", cycle = TRUE)
#     mockery::stub(logl_test, 'format.pval', mock_format_pval)
#
#     result_formatted <- logl_test(mock_model,
#                                   rand.terms = c("Block"),
#                                   numeric = FALSE)
#     expect_true(is.character(result_formatted$LogLRT.pvalue))
# })

test_that("logl_test handles convergence issues", {
    # Create model that doesn't converge initially
    mock_model <- create_mock_asreml_model(converged = FALSE)

    # Create sequence of models: non-converged, then converged
    non_converged_model <- mock_model
    converged_model <- mock_model
    converged_model$converge <- TRUE
    converged_model$loglik <- -101

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(non_converged_model, converged_model, converged_model, cycle = TRUE)
    mock_quiet_fn <- mock(non_converged_model, converged_model, converged_model, cycle = TRUE)
    mock_format_pval <- mock("0.078")

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model, rand.terms = c("Block"))

    # Should still return results even with initial convergence issues
    expect_equal(nrow(result), 1)
    expect_equal(result$Term, "Block")

    # Verify multiple update calls due to convergence issues
    expect_called(mock_update, 2)
})

test_that("logl_test preserves term order", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -102

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(reduced_model, cycle = TRUE)
    mock_format_pval <- mock("0.045", "0.123", "0.067", "0.234", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model,
                        rand.terms = c("Block:Plot", "Block"),
                        resid.terms = c("ar1(Col)", "ar1(Row)"))

    # Check that order is preserved
    expected_order <- c("Block:Plot", "Block", "ar1(Col)", "ar1(Row)")
    expect_equal(result$Term, expected_order)
})

test_that("logl_test handles decimals parameter", {
    mock_model <- create_mock_asreml_model()

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -101

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(reduced_model)
    mock_quiet_fn <- mock(reduced_model)
    mock_format_pval <- mock("0.04567")  # Will be formatted to 5 decimals

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model,
                        rand.terms = c("Block"),
                        decimals = 5)

    # Check that format.pval was called (indicating decimals was processed)
    expect_called(mock_format_pval, 1)
})

test_that("logl_test integration test", {
    mock_model <- create_mock_asreml_model(
        varcomp_boundary = c("Block"),  # One boundary term
        loglik = -150
    )

    # Create reduced model
    reduced_model <- mock_model
    reduced_model$loglik <- -152
    reduced_model$vparameters <- c(0.8)

    # Mock functions
    mock_summary <- mock(list(varcomp = attr(mock_model, "summary_varcomp")))
    mock_update <- mock(mock_model, reduced_model, reduced_model, reduced_model, cycle = TRUE)
    mock_quiet_fn <- mock(mock_model, reduced_model, reduced_model, reduced_model, cycle = TRUE)
    mock_format_pval <- mock("1.0000", "0.0456", "0.1234", "0.0789", cycle = TRUE)

    mockery::stub(logl_test, 'summary', mock_summary)
    mockery::stub(logl_test, 'asreml::update.asreml', mock_update)
    mockery::stub(logl_test, 'quiet', mock_quiet_fn)
    mockery::stub(logl_test, 'format.pval', mock_format_pval)

    result <- logl_test(mock_model,
                        rand.terms = c("Block", "Block:Plot"),
                        resid.terms = c("ar1(Row)", "ar1(Col)"),
                        decimals = 4,
                        numeric = FALSE,
                        quiet = TRUE)

    # Check comprehensive results
    expect_equal(nrow(result), 3)
    expect_equal(result$Term, c("Block:Plot", "ar1(Row)", "ar1(Col)"))
    expect_true(all(c("Term", "LogLRT.pvalue") %in% colnames(result)))

    # All should have character p-values (numeric = FALSE)
    expect_true(is.character(result$LogLRT.pvalue))
})

test_that("logl_test handles zero p-values with numeric = TRUE", {
  mock_model <- create_mock_asreml_model()
  
  # Create a mock loglik_test function that returns 0 p-values
  mock_loglik_test_zero <- function(full, reduced, decimals = 3) {
    return(0)  # Return exactly 0 to trigger the replacement logic
  }
  
  with_mocked_bindings(
    summary.asreml = mock_summary,
    `asreml::update.asreml` = mock_update,
    quiet = mock_quiet,
    .package = "base",
    {
      # Use stub to replace the internal loglik_test function
      stub(logl_test, "loglik_test", mock_loglik_test_zero)
      
      result <- logl_test(mock_model,
                         rand.terms = c("Block"),
                         decimals = 5,
                         numeric = TRUE)  # This triggers the else branch
      
      # Check that zero p-values were replaced
      expected_min_pval <- max(as.numeric("1e-5"), .Machine$double.eps)
      expect_equal(result$LogLRT.pvalue, expected_min_pval)
      expect_true(is.numeric(result$LogLRT.pvalue))
      expect_true(result$LogLRT.pvalue > 0)
    }
  )
})
