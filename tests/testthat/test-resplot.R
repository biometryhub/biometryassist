test_that("Residual plots work for aov", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

    p1 <- resplot(dat.aov, shapiro = FALSE)

    vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p1)
})

test_that("resplt is deprecated and produces a warning", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

    expect_warning(p1 <- resplt(dat.aov), "resplt has been deprecated in version 1\\.0\\.1 and will be removed in a future version\\.\\nPlease use resplot\\(\\) instead\\.")
    vdiffr::expect_doppelganger(title = "Resplot for aov", p1)
})

test_that("resplot produces an error for invalid data types", {
    expect_error(resplot(1:10), "model\\.obj must be a linear \\(mixed\\) model object\\. Currently supported model types are\\: aov, lm, lmerMod, lmerModLmerTest, asreml, mmer or art")
})

test_that("Old mod.obj argument produces a warning", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
    expect_warning(p <- resplot(model.obj = dat.aov, mod.obj = dat.aov),
                   "Argument `mod\\.obj` has been deprecated and will be removed in a future version\\. Please use `model\\.obj` instead\\.")
    vdiffr::expect_doppelganger(title = "Resplot after warning", p)
})

test_that("Residual plots work for asreml", {
    skip_on_cran()

    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    p1_single <- resplot(model.asr, shapiro = FALSE, call = T)
    expect_warning(
        expect_warning(
            expect_warning(
                expect_warning(p1_multi <- resplot(complex_model.asr),
                               "Removed 1 row containing non-finite outside the scale range"),
                "Removed 1 row containing non-finite outside the scale range"),
            "Removed 1 row containing non-finite outside the scale range"),
        "Removed 1 row containing missing values or values outside the scale range")

    vdiffr::expect_doppelganger(title = "Resplot for asreml single", p1_single)
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi[[1]])
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p1_multi[[2]])
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p1_multi[[3]])
})


test_that("Residual plots work for lme4", {
    skip_if_not_installed("lme4")
    dat.lme4 <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)

    p1 <- resplot(dat.lme4, call = T)
    vdiffr::expect_doppelganger(title = "Resplot for lme4", p1)
})


test_that("Residual plots work for nlme", {
    skip_if_not_installed("nlme")
    dat.nlme <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                           data = Loblolly,
                           fixed = Asym + R0 + lrc ~ 1,
                           random = Asym ~ 1,
                           start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

    p1 <- resplot(dat.nlme, call = T)

    vdiffr::expect_doppelganger(title = "Resplot for nlme", p1)
})

test_that("Residual plots work for sommer", {
    skip_if_not_installed("sommer")
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    library(sommer)

    skip_on_os("linux")
    expect_message(dat.som <- mmer(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                                   random = ~ Blocks + Blocks:Wplots,
                                   rcov = ~ units,
                                   data = dat, verbose = FALSE),
                   "This function has been deprecated\\. Please start using 'mmes' and its auxiliary functions")
    dat.som2 <- mmes(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                     random = ~ Blocks + Blocks:Wplots,
                     rcov = ~ units,
                     data = dat, verbose = FALSE)

    p1 <- resplot(dat.som, call = T)
    p2 <- resplot(dat.som2, call = T)
    vdiffr::expect_doppelganger(title = "Resplot for sommer mmer", p1)
    vdiffr::expect_doppelganger(title = "Resplot for sommer mmes", p2)
})

test_that("Residual plots display call for aov and lm", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
    p1 <- resplot(dat.aov, call = TRUE)
    p2 <- resplot(dat.aov, call = TRUE, call.size = 7)

    vdiffr::expect_doppelganger(title = "Resplot with call", p1)
    vdiffr::expect_doppelganger(title = "Resplot with smaller call", p2)
})

test_that("Residual plots work for ARTool models", {
    load(test_path("data", "art_dat.Rdata"))
    skip_if_not_installed("ARTool")
    model.art <- ARTool::art(medmolarity ~ name + (1|rep), data = dat.art)
    p1 <- resplot(model.art)
    p2 <- resplot(model.art, call = TRUE)

    vdiffr::expect_doppelganger(title = "ARTool resplot", p1)
    vdiffr::expect_doppelganger(title = "ARTool resplot with call", p2)
})

test_that("Shapiro-Wilk test produces a warning with large numbers of observations.", {
    load(test_path("data", "large_data.Rdata"))
    dat_large.aov <- aov(y ~ x, data = large_dat)
    dat_med.aov <- aov(y ~ x, data = med_dat)

    expect_warning(p1 <- resplot(dat_large.aov, shapiro = TRUE),
                   "Shapiro-Wilk test p-values are unreliable for more than 5000 observations and has not been performed")
    expect_warning(p2 <- resplot(dat_med.aov, shapiro = TRUE),
                   "Shapiro-Wilk test p-values are unreliable for large numbers of observations")

    vdiffr::expect_doppelganger(title = "Medium data shapiro", p2)
    skip_on_os("linux")
    vdiffr::expect_doppelganger(title = "Large data shapiro", p1)
})

equivalent_ggplot2 <- function(x, y) {
    # Create temporary files that will be automatically cleaned up when the function exits
    tmp1 <- withr::local_tempfile(fileext = ".svg")
    tmp2 <- withr::local_tempfile(fileext = ".svg")

    # Save the ggplot2 objects to the temporary SVG files
    suppressMessages(ggplot2::ggsave(tmp1, plot = x))
    suppressMessages(ggplot2::ggsave(tmp2, plot = y))

    # Compare the MD5 checksums of the two files
    tools::md5sum(tmp1) == tools::md5sum(tmp2)
}

test_that("onepage is ignored for single plots", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)

    p1 <- resplot(dat.aov)
    p2 <- resplot(dat.aov, onepage = TRUE)
    expect_type(p1, "list")
    expect_type(p2, "list")
    expect_equal(class(p1), c("gg", "ggplot"))
    expect_equal(class(p2), c("gg", "ggplot"))

    expect_true(equivalent_ggplot2(p1, p2))
    vdiffr::expect_doppelganger(title = "resplot_onepage_false", p1)
    vdiffr::expect_doppelganger(title = "resplot_onepage_true", p2)
})

test_that("onepage produces plots with up to 6 on a page", {
    load(test_path("data", "multi_dsum.Rdata"))
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

    vdiffr::expect_doppelganger(title = "Onepage_off_1", p1[[1]])
    vdiffr::expect_doppelganger(title = "Onepage_off_2", p1[[2]])
    vdiffr::expect_doppelganger(title = "Onepage_off_3", p1[[3]])
    vdiffr::expect_doppelganger(title = "Onepage_on", p2)
    vdiffr::expect_doppelganger(title = "Onepage_on_page_1", p3[[1]])
    vdiffr::expect_doppelganger(title = "Onepage_on_page_2", p3[[2]])
})

test_that("onepage_col produces plots with up to 6 on a page", {
    p1 <- suppressWarnings(resplot(complex_model.asr, onepage = TRUE, onepage_cols = 3))
    p2 <- suppressWarnings(resplot(complex_model.asr, onepage = TRUE, onepage_cols = 2))

    expect_equal(length(p1), length(p2))

    expect_false(equivalent_ggplot2(p1[[1]], p2[[1]]))

    vdiffr::expect_doppelganger(title = "Onepage_cols_3", p1)
    vdiffr::expect_doppelganger(title = "Onepage_cols_2", p2)
})
