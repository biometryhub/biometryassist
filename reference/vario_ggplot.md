# Build the 2D variogram heatmap

Internal helper for
[`variogram()`](https://biometryhub.github.io/biometryassist/reference/variogram.md).
Builds the deterministic 2D heatmap/contour `ggplot2` panel from an
interpolated grid (see
[`vario_interp()`](https://biometryhub.github.io/biometryassist/reference/vario_interp.md)).
This is the lower panel of the composite variogram plot.

## Usage

``` r
vario_ggplot(gdat, row, column, palette)
```

## Arguments

- gdat:

  An interpolated grid data frame with columns `x`, `y` and `z`, as
  returned by
  [`vario_interp()`](https://biometryhub.github.io/biometryassist/reference/vario_interp.md).

- row, column:

  Axis labels for the row and column lag directions.

- palette:

  A colour palette string (see
  [`variogram()`](https://biometryhub.github.io/biometryassist/reference/variogram.md)).

## Value

A `ggplot2` object.
