# Get the response label from a model formula

Get the response label from a model formula

## Usage

``` r
response_label(model.obj)
```

## Arguments

- model.obj:

  A fitted model object with a
  [`stats::formula()`](https://rdrr.io/r/stats/formula.html) method.

## Value

The left-hand side of the model formula as a string, for use as the plot
label.
