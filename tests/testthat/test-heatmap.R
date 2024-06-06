set.seed(42)
dat <- expand.grid(x = 1:5, y = 1:6)
dat$value <- rnorm(30)
dat$groups <- sample(rep(LETTERS[1:6], times = 5))

test_that("Invalid data argument produces an error", {
    expect_error(heat_map("abc", value, x, y),
                 "abc is not a data frame\\.")
})

test_that("heat_map produces a ggplot object", {
    hm <- heat_map(dat, value, x, y)
    expect_s3_class(hm, "gg")
    expect_s3_class(hm, "ggplot")
})

test_that("heat_map produces an image", {
    vdiffr::expect_doppelganger(title = "Heatmap of generated data",
                                heat_map(dat, value, x, y))
})


test_that("NSE produces identical results", {
    hm1 <- heat_map(dat, "value", "x", "y")
    hm2 <- heat_map(dat, value, x, y)
    expect_identical(hm1, hm2)
})

test_that("heat_map produces a grouped plot", {
    vdiffr::expect_doppelganger(title = "Heatmap of grouped data",
                                heat_map(dat, value, x, y, groups))
})

test_that("heat_map produces a grouped plot", {
    vdiffr::expect_doppelganger(title = "Heatmap of grouped data",
                                heat_map(dat, value, x, y, groups))
})

test_that("raster=FALSE uses geom_tile", {
    vdiffr::expect_doppelganger(title = "Heatmap using geom_tile",
                                heat_map(dat, value, x, y, raster = FALSE))
})

test_that("smooth argument produces an interpolated heat_map", {
    vdiffr::expect_doppelganger(title = "Heatmap of interpolated data",
                                heat_map(dat, value, x, y, smooth = TRUE))
    vdiffr::expect_doppelganger(title = "Heatmap of interpolated grouped data",
                                heat_map(dat, value, x, y, groups, smooth = TRUE))
})

test_that("palette argument changes the plot palette", {
    vdiffr::expect_doppelganger(title = "Heatmap with spectral palette",
                                heat_map(dat, value, x, y, palette = "Spectral"))
    vdiffr::expect_doppelganger(title = "Heatmap with red-green palette",
                                heat_map(dat, value, x, y, palette = "Red-Green"))
})

test_that("Additional arguments are passed through to facet_wrap", {
    vdiffr::expect_doppelganger(title = "Heatmap with facet labels",
                                heat_map(dat, value, x, y, groups, labeller = ggplot2:::label_both))
    vdiffr::expect_doppelganger(title = "Heatmap with free_y",
                                heat_map(dat, value, x, y, groups, scales = "free_y"))
    vdiffr::expect_doppelganger(title = "Heatmap with one row",
                                heat_map(dat, value, x, y, groups, nrow = 1))
})

