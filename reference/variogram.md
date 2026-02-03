# Display variogram plots for spatial models

Produces variogram plots for checking spatial trends.

## Usage

``` r
variogram(
  model.obj,
  row = NA,
  column = NA,
  horizontal = TRUE,
  palette = "rainbow",
  onepage = FALSE
)
```

## Arguments

- model.obj:

  An `asreml` model object.

- row:

  A row variable.

- column:

  A column variable.

- horizontal:

  Logical (default `TRUE`). The direction the plots are arranged. The
  default `TRUE` places the plots above and below, while `FALSE` will
  place them side by side.

- palette:

  A string specifying the colour scheme to use for plotting. The default
  value (`"default"`) is equivalent to `"rainbow"`. Colour blind
  friendly palettes can also be provided via options `"colo(u)r blind"`
  (both equivalent to `"viridis"`), `"magma"`, `"inferno"`, `"plasma"`,
  `"cividis"`, `"rocket"`, `"mako"` or `"turbo"`. The `"Spectral"`
  palette from
  [`scales::brewer_pal()`](https://scales.r-lib.org/reference/pal_brewer.html)
  is also possible.

- onepage:

  Logical (default FALSE). If TRUE and there are multiple groups,
  combines up to 6 plots onto a single page using a grid layout.

## Value

A `ggplot2` object.

## References

S. P. Kaluzny, S. C. Vega, T. P. Cardoso, A. A. Shelly, "S+SpatialStats:
User’s Manual for Windows® and UNIX®" *Springer New York*, 2013, p. 68,
https://books.google.com.au/books?id=iADkBwvario_pointsQBAJ.

A. R. Gilmour, B. R. Cullis, A. P. Verbyla, "Accounting for Natural and
Extraneous Variation in the Analysis of Field Experiments." *Journal of
Agricultural, Biological, and Environmental Statistics 2, no. 3*, 1997,
pp. 269–93, https://doi.org/10.2307/1400446.

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
variogram(model.asr)
} # }
```
