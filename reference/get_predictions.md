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
get_predictions(model.obj, classify, ...)
```

## Arguments

- model.obj:

  A fitted model object of a supported class (see *Supported model
  types* below).

- classify:

  Name of the predictor variable(s) as a string.

- ...:

  Additional arguments passed to the class-specific method (e.g.
  ASReml-R [`predict()`](https://rdrr.io/r/stats/predict.html)
  arguments).

## Value

A list with elements `predictions`, `sed`, `df`, `ylab`, `aliased_names`
and `classify` (and `emmeans_grid` for emmeans-backed engines).
`classify` is the input resolved to the factor names the predictions are
labelled by, in the order given by the user (e.g. ASReml-R wrappers
removed: `"at(Year):Prior_crop"` becomes `"Year:Prior_crop"`). The
comparison functions take their `classify` variables from it.

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
| `aovlist` | [`stats::aov()`](https://rdrr.io/r/stats/aov.html) with an `Error()` term | Multi-stratum aov; degrees of freedom are comparison-specific (a matrix) when comparisons span strata. |
| `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | Linear mixed model. |
| `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), [`lme4breeding::lmebreed()`](https://rdrr.io/pkg/lme4breeding/man/lmeb.html) | Linear mixed model. `lmebreed()` (relationship-based) models also carry class `lmerMod`; comparisons target the fixed-effect means with Kenward-Roger degrees of freedom, and correctly reflect the relationship structure (validated against ASReml-R). |
| `lmerModLmerTest` | [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html) | As `lmerMod`, with Satterthwaite degrees of freedom. |
| `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). |
| `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; degrees of freedom are comparison-specific (a matrix) when comparisons span strata. |
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
returning a list with elements `predictions`, `sed`, `df`, `ylab`,
`aliased_names` and `classify` (plus `emmeans_grid` for engines backed
by
[`emmeans::emmeans()`](https://rvlenth.github.io/emmeans/reference/emmeans.html)),
and add a row to the table above.

## ASReml-R terms in `classify`

For `asreml` models, `classify` names the factors to predict, as for
ASReml-R [`predict()`](https://rdrr.io/r/stats/predict.html). A term
wrapped in an ASReml-R function can be given either by its factors or as
written in the model, so for a model with `at(Year):Treatment` both
`classify = "Year:Treatment"` and `classify = "at(Year):Treatment"` give
the same result. This applies to `at()` and to the variance-structure
and relationship functions in the random model (e.g.
[`diag()`](https://rdrr.io/r/base/diag.html), `us()`, `fa()`, `vm()`),
so `fa(Site, 2):Variety` is classified with `"Site:Variety"`. Covariates
fitted with `pol()`, `spl()`, `lin()` and similar functions cannot be
compared and are not accepted in `classify`.

Predictions of terms in the random model include the random effects
(BLUPs). Their comparisons use the residual degrees of freedom, with a
warning.

For an `at()` term, ASReml-R gives a separate Wald test, with its own
denominator degrees of freedom, for each level of the conditioning
factor. A comparison within a level uses that level's degrees of
freedom; a comparison between levels uses the smaller of the two, as a
conservative choice. Levels not included in `at()` use the residual
degrees of freedom. A general contrast in
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
uses the smallest degrees of freedom among the levels it involves.

## ASReml-R prediction arguments

For `asreml` models, arguments given in `...` are passed to ASReml-R
[`predict()`](https://rdrr.io/r/stats/predict.html). Those most useful
for comparisons are:

- `present`: average only over the combinations of factor levels that
  occur in the data (see below).

- `average`: choose the factors to average over, optionally with weights
  (e.g. in proportion to replication rather than equally).

- `levels`: predict at chosen levels only, e.g. a subset of treatments,
  or at given values of a covariate rather than its mean.

- `ignore`, `use`, `except` and `only`: change which model terms enter
  the predictions, e.g. `use` to include a random term that is otherwise
  left out.

- `associate`: declare nested factors, e.g. treatments nested within
  treatment types.

`classify`, `sed` and `vcov` are set by the comparison functions and
cannot be passed. `aliased = TRUE` (predictions of non-estimable
functions) and `evaluate = FALSE` are not suitable. See ASReml-R
`?predict.asreml` for full details of each argument.

**When to use `present`.** By default ASReml-R
[`predict()`](https://rdrr.io/r/stats/predict.html) averages over every
combination of the levels of the fixed factors not in `classify`. If
some combinations were never observed (treatments that differ between
sites or years, a control outside a factorial set, or a factor fitted
only within some levels of another, as with `at()`), the predictions
that need them cannot be estimated: those levels are dropped as aliased,
with a warning, or the function stops with "All predicted values are
aliased". `present` restricts the averaging to the combinations in the
data. Give it the factors involved, usually those in `classify` and
those averaged over:

    multiple_comparisons(model.asr, classify = "Year:Prior_crop",
                         present = c("Year", "Prior_crop", "Treatment"))

Each mean is then averaged over only the combinations observed for it,
so two means can rest on different sets of levels of the other factors.
Check that this is a fair basis for the comparison.

## See also

[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md),
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md),
[`reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md)
