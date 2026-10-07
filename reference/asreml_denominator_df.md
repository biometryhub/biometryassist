# Denominator df for comparisons from an asreml model

Looks up the denominator df for `classify` in the `wald()` table. When
the term has its own row, that row's denDF is used for every comparison.

## Usage

``` r
asreml_denominator_df(classify, dendf, pp, resid_df)
```

## Arguments

- classify:

  The classify term, without ASReml-R wrappers and in the model's order.

- dendf:

  Data frame with columns `Source` and `denDF` from `wald()`.

- pp:

  Predictions data frame, one row per predicted mean.

- resid_df:

  Residual df of the model.

## Value

A single df, or a square df matrix matching the rows of `pp`.

## Details

An `at(F):X` term has no single row: `wald()` gives one per level of `F`
(`at(F, 'a'):X`, `at(F, 'b'):X`, ...), each with its own denDF. For
`classify = "F:X"`, a comparison within one level of `F` uses that
level's denDF, which is the df ASReml-R uses to test `X` at that level.
A comparison across levels has no exact df (with level-specific residual
variances it is a Welch-type problem), so the smaller of the two levels'
denDF is used as a conservative bound. Levels without a row (when `at()`
was given a subset of levels) use the residual df.

Otherwise, for example for a random term, the residual df is used with a
warning.
