# load(test_path("data", "oats_data.Rdata"), envir = .GlobalEnv)
# model.asr <- readRDS(test_path("data", "model_asr.rds"))
load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)

test_that("vario_df produces a dataframe", {
    vg <- vario_df(model.asr)
    expect_equal(nrow(vg), 72)
    expect_equal(round(vg[1:6, "gamma"], 3), c(0.000, 74.746, 109.053, 110.875, 99.564, 101.389))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")
})

test_that("variogram produces a plot", {
    # expect_warning(
        v1 <- variogram(model.asr)#,
        # "Removed 81 rows containing non-finite values \\(stat_contour\\)")
    # expect_warning(
        v2 <- variogram(model.asr, palette = "colourblind")#,
        # "Removed 81 rows containing non-finite values \\(stat_contour\\)")
    # expect_warning(
        v3 <- variogram(model.asr, palette = "colorblind")#,
        # "Removed 81 rows containing non-finite values \\(stat_contour\\)")
    # expect_warning(
        v4 <- variogram(model.asr, palette = "magma")#,
        # "Removed 81 rows containing non-finite values \\(stat_contour\\)")
    # expect_warning(
        v5 <- variogram(model.asr, palette = "Spectral")#,
        # "Removed 81 rows containing non-finite values \\(stat_contour\\)")
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


