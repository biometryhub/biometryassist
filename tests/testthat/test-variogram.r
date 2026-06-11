load(test_path("data", "asreml_model.Rdata"), .GlobalEnv)
load(test_path("data", "multi_dsum.Rdata"))

test_that("vario_df produces a dataframe", {
	vg <- vario_df(model.asr)
	expect_equal(nrow(vg), 72)
	expect_equal(
		round(vg[1:6, "gamma"], 3),
		c(0.000, 74.746, 109.053, 110.875, 99.564, 101.389)
	)
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
	expect_equal(vg$gamma[1], 0) # First gamma should be 0
	expect_true(max(vg$gamma) > 0) # Should have non-zero gammas

	# np at the (0,0) lag must equal the number of non-missing residuals - the
	# exact invariant vario_df() guarantees (nrows*ncols minus missing).
	n_obs <- sum(!is.na(residuals(model.asr)[model.asr$mf$units]))
	expect_equal(vg$np[1], n_obs)
})

test_that("vario_interp interpolates onto a regular grid", {
	vg <- vario_df(model.asr)
	gdat <- vario_interp(vg)

	# Default is a 40 x 40 regular grid with x/y coordinates and interpolated z
	expect_named(gdat, c("x", "y", "z"))
	expect_equal(nrow(gdat), 40 * 40)

	# The grid-size argument is honoured
	expect_equal(nrow(vario_interp(vg, n = 10)), 10 * 10)

	# Bilinear interpolation cannot overshoot the convex hull of the inputs, so
	# the interpolated surface stays within the range of the source gamma. This
	# is a deterministic, platform-independent check on the data feeding both
	# the heatmap and the wireframe.
	expect_true(all(is.finite(gdat$z)))
	expect_gte(min(gdat$z), min(vg$gamma))
	expect_lte(max(gdat$z), max(vg$gamma))
})

test_that("vario_ggplot maps the interpolated surface onto the heatmap", {
	vg <- vario_df(model.asr)
	gdat <- vario_interp(vg)
	a <- vario_ggplot(gdat, "Row", "Column", "rainbow")

	expect_s3_class(a, "ggplot")

	# One tile per interpolated grid cell, with the fill driven by z. Checked
	# via layer data so it runs on every platform (incl. CI), unlike the vdiffr
	# snapshots below.
	tiles <- layer_data_for(a, "GeomTile")
	expect_equal(nrow(tiles), nrow(gdat))
	expect_equal(sort(tiles$z), sort(gdat$z))

	# A contour layer is drawn over the tiles
	expect_silent(layer_data_for(a, "GeomContour"))

	# Changing the palette changes the rendered colours but not the mapped data
	a2 <- vario_ggplot(gdat, "Row", "Column", "viridis")
	tiles2 <- layer_data_for(a2, "GeomTile")
	expect_equal(tiles2$z, tiles$z)
	expect_false(identical(tiles2$fill, tiles$fill))
})

test_that("variogram produces correct plot structure", {
	v1 <- variogram(model.asr)

	# Test that it returns a plot object
	expect_contains(class(v1), "ggplot")

	# Test that the plot has the expected structure
	expect_true(!is.null(v1))
	expect_s3_class(v1, "variogram_plot")
	expect_s3_class(v1, "patchwork")

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
	expect_error(
		variogram(model.asr, palette = "abc"),
		"Invalid value for palette."
	)
})

test_that("variogram plot contains expected data layers", {
	v1 <- variogram(model.asr)

	# Extract the ggplot component (second element of the plot_grid)
	# This is a bit tricky since it's wrapped in cowplot::plot_grid
	# We'll test that we can build the plot without errors
	expect_silent(print(v1))
})

test_that("variogram heatmap visual regression", {
	# Snapshot ONLY the 2D ggplot heatmap panel (vario_ggplot()). The full
	# composite also contains a lattice 3D wireframe grob whose rendering is
	# non-deterministic even on a fixed machine, so the composite is unsuitable
	# for pixel snapshots. The wireframe shares its input grid with the heatmap,
	# so its correctness is covered by the vario_interp() data-level tests above,
	# and "the composite renders without error" is covered by the print() test.
	# The heatmap panel is deterministic, so it follows the package's standard
	# local-snapshot pattern (expect_local_doppelganger + ggplot2 variant).
	gdat <- vario_interp(vario_df(model.asr))

	expect_local_doppelganger(
		"Variogram heatmap",
		vario_ggplot(gdat, "Row", "Column", "rainbow"),
		variant = ggplot2_variant()
	)

	# A single alternative palette guards the colour-scale mapping
	expect_local_doppelganger(
		"Variogram heatmap colourblind",
		vario_ggplot(gdat, "Row", "Column", "colourblind"),
		variant = ggplot2_variant()
	)
})

test_that("vario produces an error for other models and data types", {
	model.lm <- lm(Petal.Length ~ Petal.Width, data = iris)
	expect_error(
		variogram(model.lm),
		"model.obj must be an asreml model object"
	)
	expect_error(variogram(1:3), "model.obj must be an asreml model object")
})

test_that("vario produces an error for residuals with units", {
	expect_error(
		variogram(model3.asr),
		"Residual term must include spatial component."
	)
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
	for (i in seq_along(vg_plots)) {
		expect_true(!is.null(vg_plots[[i]]))
		expect_silent(print(vg_plots[[i]]))
	}
})

test_that("variogram dsum heatmap visual regression", {
	# Snapshot the deterministic heatmap panel of the first group only - see the
	# note in the single-group heatmap snapshot test above.
	vg <- vario_df(model4.asr)
	grp1 <- vg[vg$groups == unique(vg$groups)[1], ]

	expect_local_doppelganger(
		"Variogram dsum heatmap first",
		vario_ggplot(vario_interp(grp1), "Row", "Column", "rainbow"),
		variant = ggplot2_variant()
	)
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

test_that("vario_df sets gamma to 0 when there are no valid residual pairs", {
	# Construct a minimal object that exercises the `n_total == 0` branch inside
	# the lag loop, covering: `gammas[index] <- 0`
	fake_model <- list(
		R.param = structure(list(1), names = "Row:Column"),
		mf = data.frame(
			Row = c(1, 1, 2, 2),
			Column = c(1, 2, 1, 2),
			units = 1:4
		),
		residuals = rep(NA_real_, 4)
	)

	vg <- vario_df(fake_model)

	# With all residuals missing, there are no valid pairs for any lag
	expect_true(all(vg$np == 0))
	# gamma should be set to 0 for the (0,0) case and for all lags
	expect_true(all(vg$gamma == 0))
	# Specifically ensure we exercised a non-(0,0) lag
	expect_equal(vg$gamma[2], 0)
})
