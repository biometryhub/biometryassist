# Create buffers for design plots

Create buffers for design plots

## Usage

``` r
create_buffers(design, type, by = NULL)
```

## Arguments

- design:

  The data frame of the design.

- type:

  The type of buffer. One of edge, row, column, double row, double
  column, blocks (internal boundaries only), or double block/entire
  block/full block (buffers fully surrounding each block).

- by:

  The name of a grouping column. Required for `type = "block"` and the
  `"double block"` family (buffers are placed at the boundaries of this
  column's groups). Optional for `type = "row"`/`"col"`: when supplied,
  a buffer is inserted only where the `by` group changes between
  adjacent rows/columns (rather than between every pair); when `NULL`, a
  buffer is inserted between every pair. Ignored by `edge`,
  `double row`, and `double col`. The `edge`/`row`/`col` (and double)
  types still assign buffer cells to blocks by position when the design
  has a `"block"` column.

## Value

The original data frame, updated to include buffers
