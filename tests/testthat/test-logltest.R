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

