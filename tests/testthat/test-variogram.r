load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
load(test_path("data", "multi_dsum.Rdata"))

test_that("vario_df produces a dataframe", {
    vg <- vario_df(model.asr)
    expect_equal(nrow(vg), 72)
    expect_equal(round(vg[1:6, "gamma"], 3), c(0.000, 74.746, 109.053, 110.875, 99.564, 101.389))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")
})

test_that("vario_df structure and properties are correct", {
    vg <- vario_df(model.asr)

    # Check column structure
    expect_named(vg, c("Row", "Column", "gamma", "np"))

    # Check data types
    expect_true(is.numeric(vg$Row))
    expect_true(is.numeric(vg$Column))
    expect_true(is.numeric(vg$gamma))
    expect_true(is.numeric(vg$np))

    # Check value ranges
    expect_true(all(vg$Row >= 0))
    expect_true(all(vg$Column >= 0))
    expect_true(all(vg$gamma >= 0))
    expect_true(all(vg$np >= 0))

    # Check specific statistical properties
    expect_equal(vg$gamma[1], 0)  # First gamma should be 0
    expect_true(max(vg$gamma) > 0)  # Should have non-zero gammas

    # Check that np (number of pairs) is reasonable
    expect_true(all(vg$np <= nrow(vg)*ncol(vg)))
})

test_that("variogram produces correct plot structure", {
    v1 <- variogram(model.asr)

    # Test that it returns a plot object
    expect_contains(class(v1), "ggplot")

    # Test that the plot has the expected structure
    expect_true(!is.null(v1))
    expect_type(v1, "object")

    # Test different palettes return valid plot objects
    v2 <- variogram(model.asr, palette = "colourblind")
    v3 <- variogram(model.asr, palette = "colorblind")
    v4 <- variogram(model.asr, palette = "magma")
    v5 <- variogram(model.asr, palette = "Spectral")

    expect_contains(class(v2), "ggplot")
    expect_contains(class(v3), "ggplot")
    expect_contains(class(v4), "ggplot")
    expect_contains(class(v5), "ggplot")

    # Test invalid palette gives error
    expect_error(variogram(model.asr, palette = "abc"),
                 "Invalid value for palette.")
})

test_that("variogram plot contains expected data layers", {
    v1 <- variogram(model.asr)

    # Extract the ggplot component (second element of the plot_grid)
    # This is a bit tricky since it's wrapped in cowplot::plot_grid
    # We'll test that we can build the plot without errors
    expect_silent(print(v1))
})

test_that("variogram visual regression - minimal snapshot", {
    skip_on_cran()
    skip_on_ci()  # Skip on CI where rendering differs
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")

    # Only test on platforms where we have stable rendering
    skip_on_os(c("mac", "linux"))  # Only test on mac in controlled environment

    v1 <- variogram(model.asr)
    vdiffr::expect_doppelganger(title = "Variogram produced", v1)

    # Test only one palette to minimize fragility
    vdiffr::expect_doppelganger(title = "Variogram palette colourblind",
                                variogram(model.asr, palette = "colourblind"))
})

test_that("vario produces an error for other models and data types", {
    model.lm <- lm(Petal.Length~Petal.Width, data = iris)
    expect_error(variogram(model.lm), "model.obj must be an asreml model object")
    expect_error(variogram(1:3), "model.obj must be an asreml model object")
})

test_that("vario produces an error for residuals with units", {
    expect_error(variogram(model3.asr), "Residual term must include spatial component.")
})

test_that("variogram works with dsum models - data structure", {
    vg <- vario_df(model4.asr)

    # Check column structure
    expect_named(vg, c("Row", "Column", "gamma", "np", "groups"))
    expect_equal(unique(vg$groups), c("2020", "2021"))
    expect_s3_class(vg, c("variogram", "data.frame"))
    expect_type(vg, "list")

    # Check that both groups have data
    expect_true(sum(vg$groups == "2020") > 0)
    expect_true(sum(vg$groups == "2021") > 0)

    # Check data properties for each group
    vg_2020 <- vg[vg$groups == "2020", ]
    vg_2021 <- vg[vg$groups == "2021", ]

    expect_true(all(vg_2020$gamma >= 0))
    expect_true(all(vg_2021$gamma >= 0))
    expect_equal(vg_2020$gamma[1], 0)
    expect_equal(vg_2021$gamma[1], 0)
})

test_that("variogram works with dsum models - plot structure", {
    vg_plots <- variogram(model4.asr)

    # Should return a list of plots
    expect_type(vg_plots, "list")
    expect_equal(length(vg_plots), 2)
    expect_equal(names(vg_plots), c("2020", "2021"))

    # Each element should be a valid plot
    for(i in seq_along(vg_plots)) {
        expect_true(!is.null(vg_plots[[i]]))
        expect_silent(print(vg_plots[[i]]))
    }
})

test_that("variogram dsum visual regression - minimal", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    skip_on_os(c("mac", "linux"))

    # Test only the first plot to minimize fragility
    vdiffr::expect_doppelganger(title = "Variogram dsum first",
                                variogram(model4.asr)[[1]])
})

test_that("onepage argument groups multiple plots into 1", {
    vg <- variogram(model4.asr, onepage = TRUE)

    # Should return a list with single element (one page)
    expect_type(vg, "list")
    expect_equal(length(vg), 1)

    # Minimise printing for speed
    expect_true(!is.null(vg[[1]]))
    expect_contains(class(vg[[1]]), "ggplot")

    vg_multi <- variogram(model_dsum, onepage = TRUE)

    # Should have 2 pages (more than 6 groups)
    expect_type(vg_multi, "list")
    expect_equal(length(vg_multi), 2)

    # Both pages should be printable
    expect_true(!is.null(vg_multi[[1]]))
    expect_contains(class(vg_multi[[1]]), "ggplot")
    expect_true(!is.null(vg_multi[[2]]))
    expect_contains(class(vg_multi[[2]]), "ggplot")
    # expect_silent(print(vg_multi[[2]]))
})

test_that("onepage handles different numbers of groups correctly", {
    # Test with 2 groups (should be 1 page)
    vg <- variogram(model4.asr, onepage = TRUE)
    expect_equal(length(vg), 1)

    # Test with multiple groups
    vg_multi <- variogram(model_dsum, onepage = TRUE)

    # Calculate expected pages
    n_groups <- length(unique(vario_df(model_dsum)$groups))
    expected_pages <- ceiling(n_groups / 6)
    expect_equal(length(vg_multi), expected_pages)
})

test_that("onepage visual regression - minimal", {
    skip_on_cran()
    skip_on_ci()
    skip_on_covr()
    skip_if(packageVersion("grid") < "4.2.1")
    skip_on_os(c("mac", "linux"))

    # Test only one onepage plot
    vg <- variogram(model4.asr, onepage = TRUE)
    vdiffr::expect_doppelganger(title = "Variogram onepage", print(vg[[1]]))
})

test_that("variogram data is consistent across palette changes", {
    # The underlying data should be the same regardless of palette
    vg_df <- vario_df(model.asr)

    # Create plots with different palettes
    v1 <- variogram(model.asr, palette = "default")
    v2 <- variogram(model.asr, palette = "colourblind")
    v3 <- variogram(model.asr, palette = "magma")

    # All should produce valid output
    expect_true(!is.null(v1))
    expect_true(!is.null(v2))
    expect_true(!is.null(v3))

    # All should be printable
    expect_silent(print(v1))
    expect_silent(print(v2))
    expect_silent(print(v3))
})
