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
    expect_warning(p <- resplot(mod.obj = dat.aov),
                   "mod.obj has been deprecated to be consistent with other functions. Please use `model.obj` instead.")
    vdiffr::expect_doppelganger(title = "Resplot after warning", p)
})

test_that("Residual plots work for asreml", {
    skip_if_not(requireNamespace("asreml", quietly = TRUE))

    dat.asr <- quiet(asreml::asreml(Petal.Length ~ Petal.Width, data = iris, trace = FALSE))
    p1_single <- resplot(dat.asr, shapiro = FALSE, call = T)

    final.m.asr <- readRDS(test_path("data", "complex_model.rds"))
    p1_multi <- suppressWarnings(resplot(final.m.asr))

    # expect_plots_identical("test-plot.png", "../figs/resplt/resplt-aov.png")
    vdiffr::expect_doppelganger(title = "Resplot for asreml single", p1_single)
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 1", p1_multi[[1]])
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 2", p1_multi[[2]])
    vdiffr::expect_doppelganger(title = "Resplot for asreml pt 3", p1_multi[[3]])


    # vdiffr::manage_cases()
})


test_that("Residual plots work for lme4", {
    skip_if_not_installed("lme4")
    dat.lme4 <- lme4::lmer(Reaction ~ Days + (Days | Subject), lme4::sleepstudy)

    # expect_error(resplot(1:10), "mod.obj must be an aov, lm, lmerMod, lmerModLmerTest, asreml or mmer object")
    p1 <- resplot(dat.lme4, call = T)
    # p2 <- resplot(dat.lme4, shapiro = FALSE)

    vdiffr::expect_doppelganger(title = "Resplot for lme4", p1)
    # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})


test_that("Residual plots work for nlme", {
    skip_if_not_installed("nlme")
    dat.nlme <- nlme::nlme(height ~ SSasymp(age, Asym, R0, lrc),
                           data = Loblolly,
                           fixed = Asym + R0 + lrc ~ 1,
                           random = Asym ~ 1,
                           start = c(Asym = 103, R0 = -8.5, lrc = -3.3))

    p1 <- resplot(dat.nlme, call = T)

    vdiffr::expect_doppelganger(title = "Resplot for nlme", dat.nlme)
    # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})

test_that("Residual plots work for sommer", {
    skip_if_not_installed("sommer")
    load(test_path("data", "asreml_model.Rdata"), envir = .GlobalEnv)
    dat.som <- sommer::mmer(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                            random = ~ Blocks + Blocks:Wplots,
                            rcov = ~ units,
                            data = dat)

    p1 <- resplot(dat.som, call = T)
    # p2 <- resplot(dat.aov, shapiro = FALSE)

    vdiffr::expect_doppelganger(title = "Resplot for sommer", p1)
    # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})

test_that("Residual plots display call for aov and lm", {
    dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
    p1 <- resplot(dat.aov, call = TRUE)
    p2 <- resplot(dat.aov, call = TRUE, call.size = 7)

    vdiffr::expect_doppelganger(title = "Resplot with call", p1)
    vdiffr::expect_doppelganger(title = "Resplot with smaller call", p2)
    # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})

test_that("Residual plots work for ARTool models", {
    load(test_path("data", "art_dat.Rdata"))
    skip_if_not_installed("ARTool")
    model.art <- ARTool::art(medmolarity ~ name + (1|rep), data = dat.art)
    p1 <- resplot(model.art)
    p2 <- resplot(model.art, call = TRUE)

    vdiffr::expect_doppelganger(title = "ARTool resplot", p1)
    vdiffr::expect_doppelganger(title = "ARTool resplot with call", p2)
    # vdiffr::expect_doppelganger(title = "Resplot for aov without shapiro", p2)
})
