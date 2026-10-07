# Build predictions, SED and df from an emmeans-backed model

Shared core for the emmeans-backed
[`get_predictions()`](https://biometryhub.github.io/biometryassist/reference/get_predictions.md)
methods (`aovlist`, `afex_aov`, ...). Given the emmeans reference grid
for `classify`, it builds the predicted means, the SED matrix and the
degrees of freedom from the pairwise contrasts, and processes aliased
levels. The df is a single value when every comparison shares it, and a
comparison-specific matrix otherwise.

## Usage

``` r
predictions_from_emmeans(
  model.obj,
  classify,
  model_terms = attr(stats::terms(model.obj), "term.labels"),
  ylab = response_label(model.obj)
)
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
  Defaults to the term labels of `model.obj`.

- ylab:

  Response variable label for the plot. Defaults to the left-hand side
  of the model formula.

## Value

A list with elements `predictions`, `sed`, `df`, `ylab`,
`aliased_names`, `emmeans_grid`, `vcov` and `classify`.
