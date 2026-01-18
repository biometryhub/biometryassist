# Visualise a graphical summary of variables from a data frame

Variables are plotted in different ways according to the number of
explanatory variables provided as input.

## Usage

``` r
summary_graph(data, response, exp_var, resp_units = "")
```

## Arguments

- data:

  A data frame containing the variables to be plotted.

- response:

  The response variable to plot.

- exp_var:

  The explanatory (or grouping) variable(s) to plot. Up to three can be
  provided.

- resp_units:

  A string providing units to display on the response variable (y) axis.
  Will use the empty string by default so axes will have no units by
  default.

## Value

A ggplot2 plot object

## Details

With a single explanatory variable, a boxplot grouped by `exp_var` is
produced. With two explanatory variables, a dot-plot with lines
connecting the mean of each group is produced, with the first element of
`exp_var` used as the x axis variable, and the second is used to colour
the points. Three explanatory variables produces the same as two, but
with the third used to facet the plot.

## Examples

``` r
summary_graph(iris, "Petal.Length", "Species", "mm")


# Multiple explanatory variables can be provided as a vector
summary_graph(npk, "yield", c("N", "P"), "lb/plot")


summary_graph(npk, "yield", c("N", "P", "K"), "lb/plot")

```
