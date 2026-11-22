# Extending `compare_transformations()` to other model engines

Implementation notes for #212 (follow-up to #211). Working document for the
`compare-transformations-engines` branch, not package documentation. If
committed, add `^COMPARE-TRANSFORMATIONS-ENGINES\.md$` to `.Rbuildignore`.

## What an engine needs

The comparison is valid for any engine that can provide all three of:

1. **Refit with a substituted response**, keeping everything else in the model
   identical (fixed and random terms, variance structures, data rows).
2. **A Gaussian likelihood** on the transformed scale.
3. **A log-likelihood reported consistently across refits.** Only differences
   between refits of the *same* engine are used, so constants an engine omits
   cancel. Comparing log-likelihoods across engines is never valid.

The normalisation (fitting `z = g(y) / exp(mean(log|g'(y)|))`) holds under
both ML and REML. REML is fine here because the fixed effects are identical for
every transformation.

## What is already engine-agnostic

No changes are needed to these: `trans_specs()`, `parse_trans()`,
`trans_values()`, `trans_response()`, `trans_auto_offset()`,
`trans_failed_fit()`, `bc_summarise()`, `bc_suggest()`, the labelling helpers,
`print.compare_transformations()` and `autoplot.compare_transformations()`.
They all work from the per-fit result list described below.

## The asreml-specific parts (line numbers as of 3d24e64)

| Location | What it does | What it becomes |
|---|---|---|
| `R/compare_transformations.R:167-170` | Class check, `check_installed("asreml")` | Dispatch; per-method `check_installed()` |
| `:185-202` | `match.call(asreml::asreml, ...)`; requires `data`, rejects `family` and `subset` | Per-method "prepare" step |
| `:224-227` | Rewrites the call: `asreml::asreml`, `fixed = .ct_y ~ .`, `data = .ct_dat`, `trace = FALSE` | Per-method: the formula argument and quiet flag differ by engine |
| `:229-241` | Keeps `G.param`/`R.param` from the original call (warm starts or fixed parameters) | asreml only; other engines have their own `start` mechanisms, or none |
| `:242-251` | Warns about fixed variance parameters (`bound == "F"`) | Per-method, where an analogue exists (`lme` fixed `varStruct`, `glmmTMB` `map`) |
| `:322-337` | Integrity check: the untransformed refit should reproduce `model.obj$loglik` | Needs a per-method "log-likelihood of the original fit" |
| `:615-652` `refit_transformed()` | Fits, restarts from final values until converged, extracts `loglik`, `converge`, `n_bound`, `varcomp` | The main per-method generic |

## Proposed structure

Two internal S3 generics (no export needed), dispatching on `model.obj`:

- **`ct_prepare(model.obj, env)`** validates the model, then returns the
  rewritten call template, the data, the fixed formula (for
  `trans_response()`), the original log-likelihood for the integrity check,
  and any engine-specific start values or warnings.
- **`ct_refit(model.obj, cl, dat, env, max_refits, start)`** fits one
  transformed response and returns the existing result contract:
  `list(loglik, converge, n_bound, note, inapplicable, varcomp)`.

The existing asreml code moves into `ct_prepare.asreml()` and
`ct_refit.asreml()` unchanged. A default method errors with the list of
supported classes. Keeping the result contract fixed means the comparison
table, profile, print and autoplot code don't change.

## Cross-cutting requirements

- **Normalise over the rows actually fitted.** `trans_values()` takes the
  geometric mean over all non-`NA` responses (`:556`). That is right for
  asreml as usually fitted, but `lm`, `lmer`, `lme` and `glmmTMB` drop rows
  with missing covariates (`na.omit`). The normalising constant must then be
  computed over the same rows, or each transformation's log-likelihood is out
  by `n * log(c)` and the comparison is biased. Pass the fitted-row index into
  `trans_values()`, e.g. from the original fit's `model.frame()`.
- **Locating the response.** The formula lives in different arguments:
  `fixed` (asreml, `lme`, `mmes`) or `formula` (`lm`, `lmer`, `glmmTMB`).
  `formula(model.obj)` plus `update(., .ct_y ~ .)` should cover most.
- **Data must come from the call** and be evaluable in `env`, as now. Keep
  rejecting `subset` (or apply it to the data first, consistently with the
  fitted-row point above).
- **`offset()` terms** in the formula are on the response scale and would be
  wrong after transformation. Reject them, or warn.
- **Convergence and restarts.** asreml restarts from its final values.
  Analogues: `lmer` via `start = getME(m, "theta")`, `glmmTMB` via `start`;
  `lme` has no simple equivalent, so report non-convergence instead.
  Convergence checks: `lmer` via `m@optinfo$conv$lme4$messages`, `glmmTMB` via
  `m$fit$convergence` and `m$sdr$pdHess`, `lme` errors on failure.
- **Boundary counts (`n_bound`).** `lmer`: `isSingular()`. `glmmTMB`: near-zero
  variance components. `lme`: no direct equivalent (use `NA`, or check for
  near-zero SDs).
- **Dependencies.** All the engines are in Enhances/Suggests, so guard each
  method with `rlang::check_installed()`.

## Engine by engine

### Easy, or close to already working

| Engine | Refit | Log-likelihood | Notes |
|---|---|---|---|
| `lm` / single-stratum `aov` | `update()` / call rewrite | `logLik()` (ML) | Validate the power profile against `MASS::boxcox()`. |
| `lmer` / `lmerTest` | call rewrite | `logLik()`, REML by default | Keep the original fit's `REML` setting. |
| `nlme::lme` | call rewrite (`fixed`) | `logLik()`, REML by default | `varPower()` etc. act on fitted values, so they're unaffected. |
| `glmmTMB`, Gaussian | call rewrite | `logLik()`; supports `REML = TRUE` | Reject non-Gaussian families (see below). A `dispformula` is fine. |
| `lme4breeding::lmebreed()` | evaluates the original `lmebreed()` call | as `lmer` | Class `lmerMod`, so the `lmerMod` method should cover it. Needs a test that the relationship matrices are carried through the call. |

### Possible with some work

| Engine | What's in the way | Approach |
|---|---|---|
| `aovlist` (`Error()` strata) | No `logLik()` method; multistratum ANOVA isn't a likelihood fit | Refit the equivalent mixed model (`lmer`/`lme`) internally and document it. A stratum-SS REML likelihood is exact only for balanced designs. |
| `afex_aov` | Between-subjects is an `lm`; within-subjects is a multivariate `mlm`, a different model | Refit the equivalent univariate mixed model, as for `aovlist`. |
| sommer `mmes()` | Not verified that it reports a converged REML log-likelihood suitable for comparison (rather than an iteration trace) | Check against an equivalent ASReml-R fit before supporting it. |

### Impossible or meaningless (informative error)

| Case | Reason to give in the error |
|---|---|
| ARTool `art` | Aligned ranks are invariant to any monotone transformation, so every transformation gives identical results. There's no likelihood either. |
| Non-Gaussian `glmmTMB` and other GLMMs | Transforming the response conflicts with the family (e.g. `log(y)` counts are no longer counts); the link function does that job. Matches the existing `family` check for asreml. |
| sommer `mmer()` (legacy) | Declined, for consistency with the comparison functions; point to `mmes()`. |

## Testing

- Add fitted fixtures to `tests/testthat/data/` for each new engine, in line
  with the existing comparison-function tests.
- `lm`: the profile maximum and interval should match `MASS::boxcox()`.
- `lmer` / `lme`: for an equivalent model, the log-likelihood *differences*
  between transformations should match the asreml method (absolute values
  won't, because of engine constants).
- Missing covariates: a test where rows are dropped by `na.omit`, to confirm
  the normalisation uses the fitted rows only.
- Error tests for `art`, non-Gaussian `glmmTMB`, `mmer` and `offset()` terms.

## Documentation

- Retitle away from "of an ASReml-R model"; update `@param model.obj`, the
  description and the `@details` wording that assumes REML.
- Keep the asreml example gated, and add an `lm` or `lmer` example that runs
  on CRAN.
- NEWS entry under Minor Changes (broadened model support), referencing #212.
