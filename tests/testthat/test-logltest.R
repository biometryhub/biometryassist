load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)

test_that("function works", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    oats.logl <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                           resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 5, quiet = TRUE)
    oats.logl2 <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                            resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 5, numeric = TRUE, quiet = TRUE)
    oats.logl3 <- logl_test(model.obj = model.asr, rand.terms = c("Blocks", "Blocks:Wplots"),
                            resid.terms = c("ar1(Row)", "ar1(Column)"), decimals = 1, quiet = TRUE)

    expect_equal(oats.logl$Term, c("Blocks", "Blocks:Wplots", "ar1(Row)", "ar1(Column)"))
    expect_equal(oats.logl$LogLRT.pvalue, c("0.05795", "0.13142", "0.00559", "0.82883"))
    expect_equal(oats.logl2$LogLRT.pvalue, c(0.05795, 0.13142, 0.00559, 0.82883))
    expect_true(is.numeric(oats.logl2$LogLRT.pvalue))
    expect_equal(oats.logl3$LogLRT.pvalue, c('0.1', '0.1', '<0.1', '0.8'))
})

test_that("logltest gives an error on different model type", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
    expect_error(logl_test(dat.aov), "Only asreml models are supported at this time.")
})

test_that("logltest gives an error on different model type", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))
    quiet(library(asreml))
    expect_error(logl_test(model.asr), "One of rand.terms or resid.terms must be provided")
})
