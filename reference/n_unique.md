# Count Unique Values

Internal helper to count the number of distinct values in a vector.
Works for numeric, character, and factor vectors.

## Usage

``` r
n_unique(x, na.rm = FALSE)
```

## Arguments

- x:

  A vector.

- na.rm:

  Logical (default `FALSE`). If `TRUE`, missing values (`NA`) are
  removed before counting.

## Value

Integer. The number of unique values in `x` (including `NA` as one
distinct value when `na.rm = FALSE`).
