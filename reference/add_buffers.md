# Add buffers to an existing design

Add buffers to an existing design

## Usage

``` r
add_buffers(design_obj, type, by = NULL)
```

## Arguments

- design_obj:

  A design object (with class "design") from the design() function

- type:

  The type of buffer to add. One of 'edge', 'row', 'column', 'double
  row', 'double column', 'blocks', or 'double block'/'entire
  block'/'full block'.

- by:

  The name of a grouping column. Used by the block-based buffer types
  (`"block"` and the `"double block"` family), and optionally by
  `"row"`/ `"column"` to insert a buffer only where the group changes
  between adjacent rows/columns (e.g. between wholeplots in a split-plot
  design). Defaults to `"block"` when the design has a `"block"` column,
  otherwise `NULL`.

## Value

The modified design object with buffers added

## Examples

``` r
# Create a simple CRD design
des <- design(type = "crd", treatments = c("A", "B"), reps = 3, nrows = 2, ncols = 3, seed = 42)
#> Source of Variation                     df
#> =============================================
#> treatments                              1
#> Residual                                4
#> =============================================
#> Total                                   5


# Plot the original design
autoplot(des)


# Add edge buffers (a border around the whole field) and plot
autoplot(add_buffers(des, type = "edge"))


# A buffer row between every pair of rows
autoplot(add_buffers(des, type = "row"))


# Two buffer rows between every pair of rows
autoplot(add_buffers(des, type = "double row"))


# For block designs, buffer the boundaries between blocks
des_rcbd <- design(type = "rcbd", treatments = LETTERS[1:4], reps = 4,
                   nrows = 4, ncols = 4, brows = 2, bcols = 2, seed = 42)
#> Source of Variation                     df
#> =============================================
#> Block stratum                           3
#> ---------------------------------------------
#> treatments                              3
#> Residual                                9
#> =============================================
#> Total                                   15


# One buffer at each internal block boundary
autoplot(add_buffers(des_rcbd, type = "block"))


# A full buffer ring surrounding each block
autoplot(add_buffers(des_rcbd, type = "double block"))


# Use `by` to buffer where a grouping column changes. In a split-plot the
# `plots` column identifies each wholeplot, so this rings every wholeplot:
des_sp <- design(type = "split", treatments = c("A", "B"), sub_treatments = 1:4,
                 reps = 4, nrows = 8, ncols = 4, brows = 4, bcols = 2, seed = 42)
#> Source of Variation                          df
#> ==================================================
#> Block stratum                                3
#> --------------------------------------------------
#> Whole plot stratum
#>          treatments                          1
#> Whole plot Residual                          3
#> ==================================================
#> Subplot stratum
#>          sub_treatments                      3
#>          treatments:sub_treatments           3
#>          Subplot Residual                   18
#> ==================================================
#> Total                                       31

autoplot(add_buffers(des_sp, type = "double block", by = "plots"))


# ... or just a buffer row between the wholeplot bands
autoplot(add_buffers(des_sp, type = "row", by = "plots"))

```
