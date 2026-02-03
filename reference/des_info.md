# Produce a graph of design layout, skeletal ANOVA table and data frame with complete design

**Deprecated**: `des_info()` has been superseded by
[`design()`](https://biometryhub.github.io/biometryassist/reference/design.md).
Please use
[`design()`](https://biometryhub.github.io/biometryassist/reference/design.md)
instead. This function will be removed in a future version.

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
