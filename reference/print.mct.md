# Print output of multiple_comparisons

Print output of multiple_comparisons

## Usage

``` r
# S3 method for class 'mct'
print(x, decimals = 2, ...)
```

## Arguments

- x:

  An mct object to print to the console.

- decimals:

  Number of decimal places to display. Default is 2.

- ...:

  Other arguments passed to print.data.frame

## Value

The original object invisibly.

## See also

[`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)

## Examples

``` r
dat.aov <- aov(Petal.Width ~ Species, data = iris)
output <- multiple_comparisons(dat.aov, classify = "Species")
print(output)
#> Multiple Comparisons of Means: Tukey's HSD Test
#> Significance level: 0.05 
#> HSD value: 0.0969097 
#> 
#> Predicted values:
#>      Species predicted.value std.error  df groups   ci  low   up
#> 1     setosa            0.25      0.03 147      a 0.06 0.19 0.30
#> 2 versicolor            1.33      0.03 147      b 0.06 1.27 1.38
#> 3  virginica            2.03      0.03 147      c 0.06 1.97 2.08
print(output, decimals = 4)
#> Multiple Comparisons of Means: Tukey's HSD Test
#> Significance level: 0.05 
#> HSD value: 0.0969097 
#> 
#> Predicted values:
#>      Species predicted.value std.error  df groups     ci    low     up
#> 1     setosa           0.246    0.0289 147      a 0.0572 0.1888 0.3032
#> 2 versicolor           1.326    0.0289 147      b 0.0572 1.2688 1.3832
#> 3  virginica           2.026    0.0289 147      c 0.0572 1.9688 2.0832
```
