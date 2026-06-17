# Build predictions, SED and df from an emmeans-backed model

Shared core for the emmeans-backed
[`get_predictions()`](https://biometryhub.github.io/biometryassist/reference/get_predictions.md)
methods (`aovlist`, `afex_aov`, ...). Given the emmeans reference grid
for `classify`, it builds the predicted means, the comparison-specific
(matrix) SED and degrees of freedom from the pairwise contrasts, and
processes aliased levels. The terms check and `ylab` are computed by the
caller (these differ per engine) and passed in.

## Usage

``` r
predictions_from_emmeans(model.obj, classify, model_terms, ylab)
```

## Arguments

- model.obj:

  A fitted model object with an
  [`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)
  method.

- classify:

  Name of the predictor variable(s) as a string.

- model_terms:

  Character vector of model term labels (for the classify check).

- ylab:

  Response variable label for the plot.

## Value

A list with elements `predictions`, `sed`, `df`, `ylab`, `aliased_names`
and `emmeans_grid`.
