# Generate automatic plots for objects generated in biometryassist

[`ggplot2::autoplot()`](https://ggplot2.tidyverse.org/reference/autoplot.html)
methods are provided for the objects created by `biometryassist`. See
the per-class methods for the available options:
[`autoplot.mct()`](https://biometryhub.github.io/biometryassist/reference/autoplot.mct.md)
for
[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
output,
[`autoplot.design()`](https://biometryhub.github.io/biometryassist/reference/autoplot.design.md)
for
[`design()`](https://biometryhub.github.io/biometryassist/reference/design.md)
output, and
[`autoplot.pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
/
[`autoplot.reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md),
which are documented alongside their respective functions.

## Usage

``` r
autoplot(object, ...)
```

## Arguments

- object:

  An object created by `biometryassist`. Methods are provided for the
  `mct`, `design`, `pairwise_comparisons` and `reference_comparisons`
  classes.

- ...:

  Arguments passed to the individual `autoplot` methods.

## Value

A `ggplot2` object.

## See also

[`autoplot.mct()`](https://biometryhub.github.io/biometryassist/reference/autoplot.mct.md),
[`autoplot.design()`](https://biometryhub.github.io/biometryassist/reference/autoplot.design.md),
[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
and
[`design()`](https://biometryhub.github.io/biometryassist/reference/design.md)
