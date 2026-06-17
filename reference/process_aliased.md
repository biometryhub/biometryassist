# Process aliased treatments in predictions

Process aliased treatments in predictions

## Usage

``` r
process_aliased(
  pp,
  sed,
  classify,
  exclude_cols = c("predicted.value", "std.error", "df", "Names"),
  vcov = NULL
)
```

## Arguments

- pp:

  Data frame of predictions

- sed:

  Standard error of differences matrix

- classify:

  Name of predictor variable

- exclude_cols:

  Column names to exclude when processing aliased names

- vcov:

  Optional variance-covariance matrix of the predictions, subset to the
  estimable rows/columns alongside `sed` when supplied (`NULL`
  otherwise).

## Value

List containing processed predictions, sed matrix, aliased names and
(when supplied) the subset `vcov`.
