# Produce a graph of design layout, skeletal ANOVA table and data frame with complete design

Produce a graph of design layout, skeletal ANOVA table and data frame
with complete design

## Usage

``` r
des_info(
  design.obj,
  nrows,
  ncols,
  brows = NA,
  bcols = NA,
  byrow = TRUE,
  fac.names = NULL,
  fac.sep = c("", " "),
  buffer = NULL,
  plot = TRUE,
  rotation = 0,
  size = 4,
  margin = FALSE,
  save = FALSE,
  savename = paste0(design.obj$parameters$design, "_design"),
  plottype = "pdf",
  return.seed = TRUE,
  quiet = FALSE,
  ...
)
```

## Arguments

- design.obj:

  An `agricolae` design object.

- nrows:

  The number of rows in the design.

- ncols:

  The number of columns in the design.

- brows:

  For RCBD only. The number of rows in a block.

- bcols:

  For RCBD only. The number of columns in a block.

- byrow:

  For split-plot only. Logical (default: `TRUE`). Provides a way to
  arrange plots within whole-plots when there are multiple possible
  arrangements.

- fac.names:

  Allows renaming of the `A` level of factorial designs (i.e. those
  using
  [`agricolae::design.ab()`](https://rdrr.io/pkg/agricolae/man/design.ab.html))
  by passing (optionally named) vectors of new labels to be applied to
  the factors within a list. See examples and details for more
  information.

- fac.sep:

  The separator used by `fac.names`. Used to combine factorial design
  levels. If a vector of 2 levels is supplied, the first separates
  factor levels and label, and the second separates the different
  factors.

- buffer:

  The type of buffer. One of edge, row, column, double row, double
  column, or block (coming soon).

- plot:

  Logical (default `TRUE`). If `TRUE`, display a plot of the generated
  design. A plot can always be produced later using
  [`autoplot()`](https://biometryhub.github.io/biometryassist/reference/autoplot.md).

- rotation:

  Rotate the text output as Treatments within the plot. Allows for
  easier reading of long treatment labels. Takes positive and negative
  values being number of degrees of rotation from horizontal.

- size:

  Increase or decrease the text size within the plot for treatment
  labels. Numeric with default value of 4.

- margin:

  Logical (default FALSE). Setting to `TRUE` will add a margin (white
  space) between plot and axes.

- save:

  One of `FALSE` (default)/`"none"`, `TRUE`/`"both"`, `"plot"` or
  `"workbook"`. Specifies which output to save.

- savename:

  A filename for the design to be saved to. Default is the type of the
  design combined with "\_design".

- plottype:

  The type of file to save the plot as. Usually one of `"pdf"`, `"png"`,
  or `"jpg"`. See
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for all possible options.

- return.seed:

  Logical (default TRUE). Output the seed used in the design?

- quiet:

  Logical (default FALSE). Return the objects without printing output.

- ...:

  Additional parameters passed to
  [`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html)
  for saving the plot.

## Value

A list containing a data frame with the complete design, a ggplot object
with plot layout, the seed (if `return.seed = TRUE`), and the `satab`
object, allowing repeat output of the `satab` table via
`cat(output$satab)`.

## Details

If `save = TRUE` (or `"both"`), both the plot and the workbook will be
saved to the current working directory, with filename given by
`savename`. If one of either `"plot"` or `"workbook"` is specified, only
that output is saved. If `save = FALSE` (the default, or equivalently
`"none"`), nothing will be output.

`fac.names` can be supplied to provide more intuitive names for factors
and their levels in factorial designs. They should be specified in a
list format, for example
`fac.names = list(A_names = c("a", "b", "c"), B_names = c("x", "y", "z"))`.
This will result a design output with a column named `A_names` with
levels `a, b, c` and another named `B_names` with levels `x, y, z`. Only
the first two elements of the list will be used.

If `fac.sep` is a single element (e.g. "*"), this is used to separate
all factor labels (e.g. A_1_B_1). If it is two elements (e.g. c("",
"*")), the first element separates the factor names and their levels,
and the second level separates the two factors (e.g. A1_B1).

`...` allows extra arguments to be passed to ggsave for output of the
plot. The details of possible arguments can be found in
[`ggplot2::ggsave()`](https://ggplot2.tidyverse.org/reference/ggsave.html).

## Examples

``` r
library(agricolae)

# Completely Randomised Design
trt <- c(1, 5, 10, 20)
rep <- 5
outdesign <- design.crd(trt = trt, r = rep, seed = 42)
des.out <- des_info(design.obj = outdesign, nrows = 4, ncols = 5)
#> Source of Variation                     df
#>  =============================================
#>  trt                                     3
#>  Residual                                16
#>  =============================================
#>  Total                                   19


# Randomised Complete Block Design
trt <- LETTERS[1:11]
rep <- 4
outdesign <- design.rcbd(trt = trt, r = rep, seed = 42)
des.out <- des_info(
  design.obj = outdesign, nrows = 11,
  ncols = 4, brows = 11, bcols = 1
)
#> Source of Variation                     df
#>  =============================================
#>  Block stratum                           3
#>  ---------------------------------------------
#>  trt                                     10
#>  Residual                                30
#>  =============================================
#>  Total                                   43


# Latin Square Design
trt <- c("S1", "S2", "S3", "S4")
outdesign <- design.lsd(trt)
des.out <- des_info(design.obj = outdesign, nrows = 4, ncols = 4)
#> Source of Variation                     df
#>  =============================================
#>  Row                                     3
#>  Column                                  3
#>  trt                                     3
#>  Residual                                6
#>  =============================================
#>  Total                                   15


# Factorial Design (Crossed, Completely Randomised)
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign <- design.ab(trt, r = rep, design = "crd")
des.out <- des_info(design.obj = outdesign, nrows = 6, ncols = 3)
#> Source of Variation                     df
#>  =============================================
#>  A                                       2
#>  B                                       1
#>  A:B                                     2
#>  Residual                                12
#>  =============================================
#>  Total                                   17


# Factorial Design (Crossed, Completely Randomised), renaming factors
trt <- c(3, 2) # Factorial 3 x 2
rep <- 3
outdesign <- design.ab(trt, r = rep, design = "crd")
des.out <- des_info(design.obj = outdesign, nrows = 6, ncols = 3,
                    fac.names = list(N = c(50, 100, 150),
                                     Water = c("Irrigated", "Rain-fed")))
#> Source of Variation                     df
#>  =============================================
#>  N                                       2
#>  Water                                   1
#>  N:Water                                 2
#>  Residual                                12
#>  =============================================
#>  Total                                   17


# Factorial Design (Nested, Latin Square)
trt <- c("A1", "A2", "A3", "A4", "B1", "B2", "B3")
outdesign <- design.lsd(trt)
des.out <- des_info(design.obj = outdesign, nrows = 7, ncols = 7)
#> Source of Variation                     df
#>  =============================================
#>  Row                                     6
#>  Column                                  6
#>  trt                                     6
#>  Residual                                30
#>  =============================================
#>  Total                                   48


# Split plot design
trt1 <- c("A", "B")
trt2 <- 1:4
rep <- 4
outdesign <- design.split(trt1, trt2, r = rep)
des.out <- des_info(design.obj = outdesign, nrows = 8, ncols = 4, brows = 4, bcols = 2)
#> Source of Variation                          df
#>  ==================================================
#>  Block stratum                                3
#>  --------------------------------------------------
#>  Whole plot stratum
#>           trt1                                1
#>  Whole plot Residual                          3
#>  ==================================================
#>  Subplot stratum
#>           trt2                                3
#>           trt1:trt2                           3
#>           Subplot Residual                   18
#>  ==================================================
#>  Total                                       31
```
