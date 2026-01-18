# Export Experimental Design Layout to Excel

Converts an experimental design dataframe into a spatial layout matrix
and exports to Excel with optional colour coding by treatment.

## Usage

``` r
export_design_to_excel(
  design_df,
  value_column = "treatments",
  filename = "experimental_design.xlsx",
  palette = "default"
)
```

## Arguments

- design_df:

  A dataframe containing experimental design with 'row' and 'col'
  columns

- value_column:

  Character string specifying which column to use for layout values
  (default: "treatments")

- filename:

  Character string for Excel filename (default:
  "experimental_design.xlsx")

- palette:

  colour palette for treatments. Can be a palette name (see details) or
  vector of colours. Set to NULL to disable colouring (default:
  "default")

## Value

Invisibly returns the layout dataframe

## Details

This function takes an experimental design in long format (with row/col
coordinates) and converts it to a matrix layout that matches the spatial
arrangement of the experiment, then exports to Excel with formatting and
optional colour coding.

Valid palette options include:

- "default" - Spectral palette

- ColorBrewer palettes: "brbg", "piyg", "prgn", "puor", "rdbu", "rdgy",
  "rdylbu", "rdylgn", "spectral", "set3", "paired"

- Viridis palettes: "viridis", "magma", "inferno", "cividis", "plasma",
  "rocket", "mako", "turbo"

- colour blind friendly: "colour blind", "color blind", "cb" (uses
  viridis)

- Custom vector of colours (must match number of unique treatments)

Requires the 'openxlsx' package to be installed.

## Examples

``` r
if (FALSE) { # \dontrun{
# Export with default colours
export_design_to_excel(my_design, "treatments", "my_design.xlsx")

# Export without colours
export_design_to_excel(my_design, "treatments", "my_design.xlsx", palette = NULL)

# Export with custom palette
export_design_to_excel(my_design, "treatments", "my_design.xlsx", palette = "viridis")
} # }
```
