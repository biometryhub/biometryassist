# Calculate the variogram data frame for a model

Calculate the variogram data frame for a model

## Usage

``` r
vario_df(model.obj, Row = NA, Column = NA)
```

## Arguments

- model.obj:

  An asreml model

## Value

A data frame with the variogram for a model. The data frame contains the
spatial coordinates (typically row and column), the `gamma` for that
position and the number of points with the separation.

## Examples

``` r
if (FALSE) { # \dontrun{
library(asreml)
oats <- asreml::oats
oats <- oats[order(oats$Row, oats$Column),]
model.asr <- asreml(yield ~ Nitrogen + Variety + Nitrogen:Variety,
                    random = ~ Blocks + Blocks:Wplots,
                    residual = ~ ar1(Row):ar1(Column),
                    data = oats)
vario_df(model.asr)
} # }
```
