# Residual plots of linear models.

Produces plots of residuals for assumption checking of linear (mixed)
models.

## Usage

``` r
resplt(model.obj, shapiro = TRUE, call = FALSE, label.size = 10,
axes.size = 10, call.size = 9, mod.obj)
```

## Arguments

- model.obj:

  An `aov`, `lm`, `lme`
  ([`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html)), `lmerMod`
  ([`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html)), `asreml`
  or `mmer` (sommer) model object.

- shapiro:

  (Logical) Display the Shapiro-Wilks test of normality on the plot?

- call:

  (Logical) Display the model call on the plot?

- axes.size:

  A numeric value for the size of the axes label font size in points.

- label.size:

  A numeric value for the size of the label (A,B,C) font point size.

- call.size:

  A numeric value for the size of the model displayed on the plot.

- mod.obj:

  Deprecated to be consistent with other functions. Please use
  `model.obj` instead.

## Value

A list containing ggplot2 objects which are diagnostic plots.

## See also

[`biometryassist-deprecated`](https://biometryhub.github.io/biometryassist/reference/biometryassist-deprecated.md)
