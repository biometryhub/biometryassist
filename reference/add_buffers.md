# Add buffers to an existing design

Add buffers to an existing design

## Usage

``` r
add_buffers(design_obj, type)
```

## Arguments

- design_obj:

  A design object (with class "design") from the design() function

- type:

  The type of buffer to add. One of 'edge', 'row', 'column', 'double
  row', or 'double column'.

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


# Add edge buffers to the design
des_buf <- add_buffers(des, type = "edge")

# Plot the design with buffers
autoplot(des_buf)


# Add double row buffers
des_row_buf <- add_buffers(des, type = "double row")
autoplot(des_row_buf)

```
