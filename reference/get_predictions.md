# Internal prediction extraction for the comparison functions

`get_predictions()` is the internal generic that
[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md),
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
and
[`reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md)
use to obtain the predicted means, the standard-error-of-differences
(SED) matrix and the degrees of freedom from a fitted model. It
dispatches on the class of `model.obj`. It is not exported and is not
called directly by users; support for a new model engine is added by
writing a new `get_predictions()` method.

## Usage

``` r
get_predictions(model.obj, classify, pred.obj = NULL, ...)
```

## Arguments

- model.obj:

  A fitted model object of a supported class (see *Supported model
  types* below).

- classify:

  Name of the predictor variable(s) as a string.

- pred.obj:

  Optional precomputed prediction object (`asreml` only; otherwise
  predictions are computed internally).

- ...:

  Additional arguments passed to the class-specific method (e.g.
  ASReml-R [`predict()`](https://rdrr.io/r/stats/predict.html)
  arguments).

## Value

A list with elements `predictions`, `sed`, `df`, `ylab` and
`aliased_names` (and `emmeans_grid` for emmeans-backed engines).

## Supported model types

The comparison functions
([`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md),
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
and
[`reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md))
work with any model for which a `get_predictions()` method is defined.
These are currently:

|  |  |  |
|----|----|----|
| Model class | Fitted by | Notes |
| `aov`, `lm` | [`stats::aov()`](https://rdrr.io/r/stats/aov.html), [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | Fixed-effects linear models. |
| `aovlist` | [`stats::aov()`](https://rdrr.io/r/stats/aov.html) with an `Error()` term | Multi-stratum aov; gives comparison-specific (matrix) degrees of freedom. |
| `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | Linear mixed model. |
| `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), [`lme4breeding::lmebreed()`](https://rdrr.io/pkg/lme4breeding/man/lmeb.html) | Linear mixed model. `lmebreed()` (relationship-based) models also carry class `lmerMod`; comparisons target the fixed-effect means with Kenward-Roger degrees of freedom, and correctly reflect the relationship structure (validated against ASReml-R). |
| `lmerModLmerTest` | [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html) | As `lmerMod`, with Satterthwaite degrees of freedom. |
| `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). |
| `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; gives comparison-specific (matrix) degrees of freedom. |
| `glmmTMB` | glmmTMB `glmmTMB()` | Generalized linear mixed model. Predictions are on the link scale with asymptotic (infinite) degrees of freedom; supply `trans` to back-transform. |
| `mmes` | sommer `mmes()` | Linear mixed model, via sommer's native [`predict()`](https://rdrr.io/r/stats/predict.html). SED from the prediction covariance; asymptotic (infinite) degrees of freedom (sommer provides none). |

ARTool (`art`) models are supported by
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
but **not** by the comparison functions: the aligned rank transform
makes mean-based comparisons inappropriate. Use
[`ARTool::art.con()`](https://rdrr.io/pkg/ARTool/man/art.con.html) for
contrasts on ART models instead.

sommer `mmer` models (the legacy interface) are supported by
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
but **not** by the comparison functions: current sommer provides no
[`predict()`](https://rdrr.io/r/stats/predict.html) method for `mmer`.
Refit with [`sommer::mmes()`](https://rdrr.io/pkg/sommer/man/mmes.html)
to use the comparison functions.

To add a new engine, write a `get_predictions.<class>()` method
returning a list with elements `predictions`, `sed`, `df`, `ylab` and
`aliased_names` (plus `emmeans_grid` for engines backed by
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)),
and add a row to the table above.

## See also

[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md),
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md),
[`reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md)
