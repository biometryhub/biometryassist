load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)

test_that("vario_df produces a dataframe", {
    vg <- vario_df(model.asr)
    expect_equal(nrow(vg), 72)
    expect_equal(round(vg[1:6, "gamma"], 3), c(0.000, 74.746, 109.053, 110.875, 99.564, 101.389))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")
})

test_that("variogram produces a plot", {
        v1 <- variogram(model.asr)
        v2 <- variogram(model.asr, palette = "colourblind")
        v3 <- variogram(model.asr, palette = "colorblind")
        v4 <- variogram(model.asr, palette = "magma")
        v5 <- variogram(model.asr, palette = "Spectral")
    expect_error(variogram(model.asr, palette = "abc"),
                 "Invalid value for palette.")
    expect_type(v1, "list")
    expect_s3_class(v1, "ggplot")
    skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "Variogram produced", v1)
    vdiffr::expect_doppelganger(title = "Variogram palette 1",
                                variogram(model.asr, palette = "colourblind"))
    vdiffr::expect_doppelganger(title = "Variogram palette 2",
                                variogram(model.asr, palette = "colorblind"))
    vdiffr::expect_doppelganger(title = "Variogram palette 3",
                                variogram(model.asr, palette = "magma"))
    vdiffr::expect_doppelganger(title = "Variogram palette 4",
                                variogram(model.asr, palette = "Spectral"))
})

test_that("vario produces an error for other models and data types", {
    model.lm <- lm(Petal.Length~Petal.Width, data = iris)
    expect_error(variogram(model.lm), "model.obj must be an asreml model object")
    expect_error(variogram(1:3), "model.obj must be an asreml model object")
})

test_that("vario produces an error for residuals with units", {
    expect_error(variogram(model3.asr), "Residual term must include spatial component.")
})

test_that("variogram works with dsum models", {
    vg <- vario_df(model4.asr)
    expect_equal(colnames(vg), c("Row", "Column", "gamma", "np", "groups"))
    expect_equal(unique(vg$groups), c("2020", "2021"))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")

    # variogram plots for each year
    skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "Variogram dsum",
                                variogram(model4.asr)[[1]])
    vdiffr::expect_doppelganger(title = "Variogram dsum 2",
                                variogram(model4.asr)[[2]])
})
