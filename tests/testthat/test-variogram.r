model.asr <- readRDS(test_path("data", "model_asr.rds"))

test_that("vario_df produces a dataframe", {
    vg <- vario_df(model.asr)
    expect_equal(nrow(vg), 72)
    expect_equal(round(vg[1:6, "gamma"], 3), c(0.000, 50.171, 54.679, 91.463, 101.107, 96.750))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")
})

test_that("variogram produces a plot", {
    v1 <- variogram(model.asr)
    expect_type(v1, "list")
    expect_s3_class(v1, "ggplot")
    skip_on_os(c("windows", "mac"))
    vdiffr::expect_doppelganger(title = "Variogram produced", v1)
})

test_that("vario produces an error for other models and data types", {
    model.lm <- lm(Petal.Length~Petal.Width, data = iris)
    expect_error(variogram(model.lm), "model.obj must be an asreml model object")
    expect_error(variogram(1:3), "model.obj must be an asreml model object")
})


