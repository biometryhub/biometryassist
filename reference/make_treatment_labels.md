# Build per-row treatment labels from one or more factor columns

Shared by
[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
and
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
to turn the classify factor column(s) of a predictions data frame into a
single label per row. A single factor is returned as-is (preserving its
type); multiple factors (an interaction) are joined with `sep`. The
caller chooses the separator
([`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
uses `"_"`,
[`pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
uses `":"`) and any further processing (e.g. coercion to character).

## Usage

``` r
make_treatment_labels(pp, vars, sep)
```

## Arguments

- pp:

  A predictions data frame.

- vars:

  Character vector of factor column name(s) to combine.

- sep:

  Separator used to join the columns when `vars` has length \> 1.

## Value

A vector of labels, one per row of `pp`.
