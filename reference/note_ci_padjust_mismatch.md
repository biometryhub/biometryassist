# Note when per-comparison CIs and adjusted p-values can disagree

Shared by
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
and
[`reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md).
For non-simultaneous adjustments the confidence intervals are
per-comparison (at level `sig`) while the p-values are adjusted for
multiplicity, so an interval can exclude zero while the adjusted p-value
is not significant (or, more rarely, the reverse). Emits a one-time
explanatory [`message()`](https://rdrr.io/r/base/message.html) when this
actually occurs in the result. Dunnett intervals are the simultaneous
intervals and agree with the test by construction, so they are never
flagged; `"none"` likewise never disagrees.

## Usage

``` r
note_ci_padjust_mismatch(x, sig, method)
```

## Arguments

- x:

  A comparison table with `conf.low`, `conf.high` and `p.value`.

- sig:

  The significance level used for the intervals and tests.

- method:

  The resolved adjustment method (e.g. `"holm"`, `"dunnett"`).

## Value

Invisibly `TRUE` if a message was shown, otherwise `FALSE`.
