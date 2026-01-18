# Process aliased treatments in predictions

Process aliased treatments in predictions

## Usage

``` r
process_aliased(
  pp,
  sed,
  classify,
  exclude_cols = c("predicted.value", "std.error", "df", "Names")
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

## Value

List containing processed predictions, sed matrix and aliased names
