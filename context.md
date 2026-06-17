# Context: Modifications to multiple_comparisons()

## Objective

Modify the
[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
function to support: 1. A `by` argument for splitting comparisons across
subgroups 2. A `pairs` argument for selective pairwise comparisons 3. A
`adjust` argument for flexible p-value adjustment methods beyond Tukey’s
HSD

The default behaviour (all pairwise comparisons, Tukey adjustment) must
be preserved. All existing scaffolding — letter groupings via
`multcompLetters3`, confidence intervals, back-transformation, output
formatting, plotting — must be retained.

------------------------------------------------------------------------

## New Arguments

``` r
multiple_comparisons <- function(model.obj,
                                 classify,
                                 sig = 0.05,
                                 int.type = "ci",
                                 trans = NULL,
                                 offset = NULL,
                                 power = NULL,
                                 decimals = 2,
                                 descending = FALSE,
                                 groups = TRUE,
                                 pairs = NULL,       # NEW
                                 adjust = "tukey",   # NEW
                                 by = NULL,          # NEW
                                 plot = FALSE,
                                 ...)
```

### `adjust`

A string passed to
[`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html), or `"tukey"`
(default) to preserve current behaviour. Valid values: `"tukey"`,
`"bonferroni"`, `"holm"`, `"fdr"`, `"BH"`, `"BY"`, `"none"`.

### `pairs`

A character vector of `"TrtA-TrtB"` pair names specifying which
comparisons to test. `NULL` (default) tests all pairs. Matching should
work in either direction (i.e. `"A-B"` and `"B-A"` are equivalent).

### `by`

A character vector of column name(s) in the predictions data frame to
split comparisons over. Comparisons are run independently within each
level (or combination of levels) of the `by` variable(s). Each subgroup
is treated as a separate family of comparisons — no pooling across
groups, no cross-group p-value adjustment.

------------------------------------------------------------------------

## Key Implementation Changes

### 1. Compute raw p-values first

**Critical correctness issue:** The current `calculate_pvalue_matrix()`
uses `ptukey` and therefore returns Tukey-adjusted p-values. Passing
these to [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) would
result in double-adjustment.

The fix is to compute a **raw two-sided t-test p-value matrix** first:

``` r

t_stat <- abs(pred_i - pred_j) / sed[i, j]
p_raw  <- 2 * pt(-abs(t_stat), df = ndf)
```

Then branch: - `adjust = "tukey"`: apply `ptukey` as currently
(preserves existing behaviour) - any other method: apply
`p.adjust(p_raw_vector, method = adjust)` to the lower-triangle vector
of raw p-values

### 2. New internal function: `get_diffs_from_pvalues()`

Replaces `get_diffs()` as the source of the `diffs` named logical vector
consumed by `multcompLetters3`. Signature:

``` r
get_diffs_from_pvalues <- function(pval_matrix, sig, adjust, pairs = NULL)
```

- Extracts the lower triangle of the p-value matrix as a named vector
  (`"A-B"` format)
- If `pairs` is non-NULL, marks unspecified pairs as `NA` before
  adjustment
- Applies adjustment to non-NA values only
- Returns `diffs` (named logical vector), adjusted p-values, and pair
  names
- Untested pairs (not in `pairs`) default to `FALSE` (not significantly
  different) — **this convention must be documented**

### 3. Reorder the main pipeline

Current order:

    get_predictions → get_diffs (qtukey) → calculate_pvalue_matrix → add_letter_groups

New order:

    get_predictions → calculate_raw_pvalue_matrix → get_diffs_from_pvalues (with adjust) → add_letter_groups

### 4. `by` loop

Wrap the comparison block in a split-apply-combine loop over `by` group
levels:

``` r

by_vals     <- interaction(pp[, by, drop = FALSE], drop = TRUE)
groups_list <- split(seq_len(nrow(pp)), by_vals)

results <- lapply(groups_list, function(idx) {
  pp_g  <- pp[idx, , drop = FALSE]
  sed_g <- sed[idx, idx, drop = FALSE]   # submatrix — correct for all model types
  ndf_g <- if (is.matrix(ndf)) ndf[idx, idx, drop = FALSE] else ndf
  # run calculate_raw_pvalue_matrix, get_diffs_from_pvalues,
  # add_letter_groups etc. on pp_g / sed_g / ndf_g
  # return annotated pp_g
})

pp <- do.call(rbind, results)
```

The SED matrix from
[`get_predictions()`](https://biometryhub.github.io/biometryassist/reference/get_predictions.md)
is always a full n×n matrix across all model types (asreml, lm, aovlist,
lmerMod), so submatrix slicing with `sed[idx, idx]` is safe and correct
for all supported model types.

### 5. Output changes

- `$pairwise_pvalues`: should return **adjusted** p-values (consistent
  with reported groupings). Raw p-values may optionally be retained as
  `$raw_pvalues`.
- `$hsd`: retain when `adjust = "tukey"`; set to `NULL` for other
  methods.
- Add `$comparison_method`: records the adjustment method used.

------------------------------------------------------------------------

## Notes on Statistical Validity

- **Bonferroni / Holm**: conservative but valid under dependence —
  familywise error rate guarantee holds
- **BH / FDR**: approximately valid under positive dependence, which
  pairwise comparisons satisfy
- **Tukey**: exact for the complete set of all pairwise comparisons;
  less appropriate for selective comparisons
- For the `pairs` argument (selective comparisons), Bonferroni or Holm
  over just the selected pairs is more statistically defensible than
  Tukey
- P-value adjustment via
  [`p.adjust()`](https://rdrr.io/r/stats/p.adjust.html) assumes tests
  are passed as a flat vector; use the lower-triangle vector of the raw
  p-value matrix

------------------------------------------------------------------------

## Relevant Files (provided separately)

- `mct.R` — contains
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  and all internal helpers including `get_diffs()`,
  `calculate_pvalue_matrix()`, `add_letter_groups()`
- `prediction_methods.R` — contains
  [`get_predictions()`](https://biometryhub.github.io/biometryassist/reference/get_predictions.md)
  and its model-type dispatch methods
- `waldTest.R` — reference implementation for contrast testing and `by`
  splitting logic
- `compare.R` — reference implementation for average SED and comparison
  thresholds
