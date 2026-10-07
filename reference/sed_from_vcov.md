# Build the SED matrix from a prediction variance-covariance matrix

Build the SED matrix from a prediction variance-covariance matrix

## Usage

``` r
sed_from_vcov(vcov)
```

## Arguments

- vcov:

  Variance-covariance matrix of the predicted means.

## Value

Matrix of standard errors of difference,
`SED_ij = sqrt(V_ii + V_jj - 2 * V_ij)`. The diagonal is left for the
caller to set.
