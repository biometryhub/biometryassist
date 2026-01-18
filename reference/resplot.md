# Produce residual plots of linear models

Produces plots of residuals for assumption checking of linear (mixed)
models.

## Usage

``` r
resplot(
  model.obj,
  shapiro = TRUE,
  call = FALSE,
  label.size = 10,
  axes.size = 10,
  call.size = 9,
  onepage = FALSE,
  onepage_cols = 3,
  mod.obj
)
```

## Arguments

- model.obj:

  An `aov`, `lm`, `lme`
  ([`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)), `lmerMod`
  ([`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)), `asreml`
  or `mmer` (sommer) model object.

- shapiro:

  (Logical) Display the Shapiro-Wilk test of normality on the plot? This
  test is unreliable for larger numbers of observations and will not
  work with n \>= 5000 so will be omitted from any plots.

- call:

  (Logical) Display the model call on the plot?

- label.size:

  A numeric value for the size of the label (A,B,C) font point size.

- axes.size:

  A numeric value for the size of the axes label font size in points.

- call.size:

  A numeric value for the size of the model displayed on the plot.

- onepage:

  (Logical) If TRUE and there are multiple plots, combines up to 6 plots
  per page.

- onepage_cols:

  Integer. Number of columns to use in grid layout when onepage=TRUE.
  Default is 3.

- mod.obj:

  Deprecated to be consistent with other functions. Please use
  `model.obj` instead.

## Value

A ggplot2 object containing the diagnostic plots.

## Examples

``` r
dat.aov <- aov(Petal.Length ~ Petal.Width, data = iris)
resplot(dat.aov)

resplot(dat.aov, call = TRUE)
```
