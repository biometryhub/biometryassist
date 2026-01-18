# Deprecated functions in package biometryassist.

The functions listed below are deprecated and will be removed in the
future. When possible, alternative functions with similar functionality
are also mentioned. Help pages for deprecated functions are available at
`help("<function>-deprecated")`.

## Usage

``` r
resplt(
  model.obj,
  shapiro = TRUE,
  call = FALSE,
  label.size = 10,
  axes.size = 10,
  call.size = 9,
  mod.obj
)
```

## Value

No return value.

A list containing ggplot2 objects which are diagnostic plots. For
`resplt`, use
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md).

## resplt

Residual plots of linear models.
