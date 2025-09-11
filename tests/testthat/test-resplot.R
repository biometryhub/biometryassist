# Setup models and data once for reuse across tests
dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

# Load data files at start
load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
load(test_path("data", "sommer_models.Rdata"), envir = .GlobalEnv)
load(test_path("data", "multi_dsum.Rdata"), envir = .GlobalEnv)
load(test_path("data", "lme4_model.Rdata"), envir = .GlobalEnv)
load(test_path("data", "nlme_model.Rdata"), envir = .GlobalEnv)
load(test_path("data", "ARTool_model.Rdata"), envir = .GlobalEnv)

load(test_path("data", "large_data.Rdata"), envir = .GlobalEnv)
dat_large.aov <- aov(y ~ x, data = large_dat)
dat_med.aov <- aov(y ~ x, data = med_dat)

# Start testing
test_that("Residual plots work for aov", {
    p1 <- resplot(dat.aov, shapiro = FALSE)

    vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p1, variant = ggplot2_variant())
})

test_that("resplt is deprecated and produces a warning", {
    expect_warning(p1 <- resplt(dat.aov), "resplt has been deprecated in version 1\\.0\\.1 and will be removed in a future version\\.\\nPlease use resplot\\(\\) instead\\.")
    vdiffr::expect_doppelganger(title = "Resplot for aov", p1, variant = ggplot2_variant())
})

test_that("resplot produces an error for invalid data types", {
    expect_error(resplot(1:10),
                 "model\\.obj must be a linear \\(mixed\\) model object\\. Currently supported model types are: aov, lm, lme, lmerMod, lmerModLmerTest, asreml, mmer, mmes, art")
})

test_that("Old mod.obj argument produces a warning", {
    expect_warning(p <- resplot(model.obj = dat.aov, mod.obj = dat.aov),
                   "Argument `mod\\.obj` has been deprecated and will be removed in a future version\\. Please use `model\\.obj` instead\\.")
    vdiffr::expect_doppelganger(title = "Resplot after warning", p, variant = ggplot2_variant())
})

test_that("Residual plots work for asreml", {
    skip_on_cran()

    p1_single <- resplot(model.asr, shapiro = FALSE, call = TRUE)
    expect_contains(class(p1_single), "ggplot")

    expect_warning(
        expect_warning(
            expect_warning(
                expect_warning(p1_multi <- resplot(complex_model.asr),
                               "Removed 1 row containing non-finite outside the scale range"),
                "Removed 1 row containing non-finite outside the scale range"),
            "Removed 1 row containing non-finite outside the scale range"),
        "Removed 1 row containing missing values or values outside the scale range")

    vdiffr::expect_doppelganger(title = "Resplot for asreml single", p1_single, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi[[1]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p1_multi[[2]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p1_multi[[3]], variant = ggplot2_variant())
})


test_that("Residual plots work for lme4", {
    skip_if_not_installed("lme4")
    p1 <- resplot(dat.lme4, call = TRUE)
    vdiffr::expect_doppelganger(title = "Resplot for lme4", p1, variant = ggplot2_variant())
})


test_that("Residual plots work for nlme", {
    skip_if_not_installed("nlme")
    p1 <- resplot(dat.nlme, call = TRUE)
    vdiffr::expect_doppelganger(title = "Resplot for nlme", p1, variant = ggplot2_variant())
})

test_that("Residual plots work for sommer", {
    p1 <- resplot(model_mmer, call = TRUE)
    p2 <- resplot(model_mmes, call = TRUE)

    expect_contains(class(p1), "ggplot")
    expect_contains(class(p2), "ggplot")

    skip_on_os("linux")
    vdiffr::expect_doppelganger(title = "Resplot for sommer mmer", p1, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Resplot for sommer mmes", p2, variant = ggplot2_variant())
})

test_that("Residual plots display call for aov and lm", {
    p1 <- resplot(dat.aov, call = TRUE)
    p2 <- resplot(dat.aov, call = TRUE, call.size = 7)

    vdiffr::expect_doppelganger(title = "Resplot with call", p1, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Resplot with smaller call", p2, variant = ggplot2_variant())
})

test_that("Residual plots work for ARTool models", {
    model.art <- get_art_model()
    p1 <- resplot(model.art)
    p2 <- resplot(model.art, call = TRUE)

    vdiffr::expect_doppelganger(title = "ARTool resplot", p1, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "ARTool resplot with call", p2, variant = ggplot2_variant())
})

test_that("Shapiro-Wilk test produces a warning with large numbers of observations.", {
    models <- get_large_data_models()

    expect_warning(p1 <- resplot(models$large, shapiro = TRUE),
                   "Shapiro-Wilk test p-values are unreliable for more than 5000 observations and has not been performed")
    expect_warning(p2 <- resplot(models$med, shapiro = TRUE),
                   "Shapiro-Wilk test p-values are unreliable for large numbers of observations")

    vdiffr::expect_doppelganger(title = "Medium data shapiro", p2, variant = ggplot2_variant())
    skip_on_os("linux")
    vdiffr::expect_doppelganger(title = "Large data shapiro", p1, variant = ggplot2_variant())
})

test_that("onepage is ignored for single plots", {
    p1 <- resplot(dat.aov)
    p2 <- resplot(dat.aov, onepage = TRUE)
    expect_contains(class(p1), "ggplot")
    expect_contains(class(p2), "ggplot")

    expect_true(equivalent_ggplot2(p1, p2))
    vdiffr::expect_doppelganger(title = "resplot_onepage_false", p1, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "resplot_onepage_true", p2, variant = ggplot2_variant())
})

test_that("onepage produces plots with up to 6 on a page and column changes work", {
    # Test basic onepage functionality
    p1 <- suppressWarnings(resplot(complex_model.asr))
    p2 <- suppressWarnings(resplot(complex_model.asr, onepage = TRUE))
    p3 <- resplot(model_dsum, onepage = TRUE)

    expect_equal(length(p1), 3)
    expect_equal(length(p2), 1)
    expect_equal(length(p3), 2)

    expect_equal(names(p1), c("2018", "2019", "2020"))
    expect_null(names(p2))
    expect_null(names(p3))

    expect_equal(class(p1), "list")
    expect_equal(class(p2), "list")
    expect_equal(class(p3), "list")

    expect_false(equivalent_ggplot2(p1[[1]], p2[[1]]))

    # Test column changes in same test to avoid redundant model loading
    p4 <- suppressWarnings(resplot(complex_model.asr, onepage = TRUE, onepage_cols = 3))
    p5 <- suppressWarnings(resplot(complex_model.asr, onepage = TRUE, onepage_cols = 2))

    expect_equal(length(p4), length(p5))
    expect_false(equivalent_ggplot2(p4[[1]], p5[[1]]))

    vdiffr::expect_doppelganger(title = "Onepage_off_1", p1[[1]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_off_2", p1[[2]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_off_3", p1[[3]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_on", p2, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_on_page_1", p3[[1]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_on_page_2", p3[[2]], variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_cols_3", p4, variant = ggplot2_variant())
    vdiffr::expect_doppelganger(title = "Onepage_cols_2", p5, variant = ggplot2_variant())
})
