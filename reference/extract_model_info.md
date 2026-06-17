# Internal model-information extraction for resplot()

`extract_model_info()` is the internal generic that
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
uses to pull the residuals, fitted values and (optionally) the model
call from a fitted model. It dispatches on the class of `model.obj`. It
is not exported and is not called directly by users; support for a new
model engine is added by writing a new `extract_model_info()` method.

## Usage

``` r
extract_model_info(model.obj, call = FALSE)
```

## Arguments

- model.obj:

  A fitted model object of a supported class (see *Supported model
  types* below).

- call:

  Logical; whether to extract the model call for display.

## Value

A list with elements `facet`, `facet_name`, `resids`, `fits`, `k` and
`model_call`.

## Supported model types

[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
produces residual diagnostics for any model with an
`extract_model_info()` method. These are currently:

|  |  |  |
|----|----|----|
| Model class | Fitted by | Notes |
| `aov`, `lm` | [`stats::aov()`](https://rdrr.io/r/stats/aov.html), [`stats::lm()`](https://rdrr.io/r/stats/lm.html) | Fixed-effects linear models. |
| `aovlist` | [`stats::aov()`](https://rdrr.io/r/stats/aov.html) with an `Error()` term | Multi-stratum aov; each error stratum (except the intercept) is shown as a separate plot. |
| `lme` | [`nlme::lme()`](https://rdrr.io/pkg/nlme/man/lme.html) | Linear mixed model. |
| `lmerMod` | [`lme4::lmer()`](https://rdrr.io/pkg/lme4/man/lmer.html), [`lme4breeding::lmebreed()`](https://rdrr.io/pkg/lme4breeding/man/lmeb.html) | Linear mixed model. `lmebreed()` (relationship-based) models also carry class `lmerMod`; their residuals/fitted values are on the response scale, so the diagnostics are valid. |
| `lmerModLmerTest` | [`lmerTest::lmer()`](https://rdrr.io/pkg/lmerTest/man/lmer.html) | As `lmerMod`. |
| `asreml` | ASReml-R `asreml()` | Linear mixed model (commercial; not on CRAN). Residual strata are shown as separate plots. |
| `mmer`, `mmes` | sommer `mmer()` / `mmes()` | Linear mixed model. |
| `art` | [`ARTool::art()`](https://rdrr.io/pkg/ARTool/man/art.html) | Aligned rank transform model. |
| `afex_aov` | afex `aov_car()` / `aov_ez()` / `aov_4()` | Factorial / repeated-measures ANOVA; a single diagnostic panel from the model residuals. |
| `glmmTMB` | glmmTMB `glmmTMB()` | **Gaussian family only.** Non-Gaussian families error with a pointer to `DHARMa::simulateResiduals()`, since a normal Q-Q plot is not a valid diagnostic for them. |

This set differs slightly from the comparison functions (see
[`get_predictions()`](https://biometryhub.github.io/biometryassist/reference/get_predictions.md)):
[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
additionally supports ARTool (`art`) models and sommer's legacy `mmer`
interface. Neither is available for the comparison functions — ART uses
aligned ranks (use
[`ARTool::art.con()`](https://rdrr.io/pkg/ARTool/man/art.con.html)), and
current sommer provides no
[`predict()`](https://rdrr.io/r/stats/predict.html) for `mmer` (refit
with [`sommer::mmes()`](https://rdrr.io/pkg/sommer/man/mmes.html)).

To add a new engine, write an `extract_model_info.<class>()` method
returning a list with elements `facet`, `facet_name`, `resids`, `fits`,
`k` and `model_call`, and add a row to the table above.

## See also

[`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
