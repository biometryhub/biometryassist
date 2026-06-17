# Interpolate variogram points onto a regular grid

Internal helper for
[`variogram()`](https://biometryhub.github.io/biometryassist/reference/variogram.md).
Takes the variogram points for a single group (as produced by
[`vario_df()`](https://biometryhub.github.io/biometryassist/reference/vario_df.md))
and interpolates the `gamma` surface onto a regular `n` by `n` grid
using
[`pracma::interp2()`](https://rdrr.io/pkg/pracma/man/interp2.html). The
returned grid is the shared input to both the 2D heatmap
([`vario_ggplot()`](https://biometryhub.github.io/biometryassist/reference/vario_ggplot.md))
and the 3D wireframe.

## Usage

``` r
vario_interp(points, n = 40)
```

## Arguments

- points:

  A data frame of variogram points for a single group. The spatial
  coordinates are expected in the first two columns (column 1 maps to
  `y`, column 2 to `x`) and the semivariance in a `gamma` column.

- n:

  The number of grid points along each axis (default `40`).

## Value

A data frame with columns `x`, `y` and `z` (the interpolated `gamma`),
containing `n * n` rows.
