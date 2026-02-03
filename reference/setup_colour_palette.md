# Setup Colour Palette for Plotting

Internal helper function to generate or validate colour palettes for
experimental design plots. Supports predefined palettes (ColorBrewer,
Viridis) or custom colours.

## Usage

``` r
setup_colour_palette(palette, n)
```

## Arguments

- palette:

  Either a single string naming a predefined palette or a vector of
  custom colours

- n:

  Integer number of required palette length

## Value

Character vector of hex colour codes of length `n`
