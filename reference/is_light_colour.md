# Determine if a Colour is Light

Internal helper function to determine whether a colour is light or dark
for appropriate font colour selection (black text on light backgrounds,
white text on dark backgrounds).

## Usage

``` r
is_light_colour(colour)
```

## Arguments

- colour:

  A colour specification (hex code, named colour, etc.)

## Value

Logical. TRUE if the colour is light (luminance \> 0.5), FALSE if dark.

## Details

Uses standard luminance calculation: 0.299*R + 0.587*G + 0.114\*B,
normalized to 0-1 scale. Coefficients reflect human eye sensitivity to
different colours (green \> red \> blue).
