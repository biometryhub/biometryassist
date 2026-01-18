# biometryassist

The goal of biometryassist is to provide functions to aid in the Design
and Analysis of Agronomic-style experiments through easy access to
documentation and helper functions, especially while teaching these
concepts.

*This package is a renamed version of BiometryTraining which is no
longer maintained, but can still be found at
<https://biometryhub.github.io/BiometryTraining/>*

------------------------------------------------------------------------

## Installation

As of version 1.0.0 the biometryassist package is now [on
CRAN](https://cran.r-project.org/package=biometryassist) 🙌 That means
that installation is as easy as running:

``` r
install.packages("biometryassist")
```

### Development version

⚠ **Warning**: The development version is unstable and liable to change
more often than the CRAN version. It may have bugs fixed, but there may
be other currently unknown bugs introduced. ⚠

Use the following code to install the latest development version of this
package.

``` r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("biometryhub/biometryassist@dev")
```

## Using the package

Load the package and start using it with:

``` r
library(biometryassist)
```

If you find this package useful, please cite it! Type
`citation("biometryassist")` on the R console to find out how.

## Troubleshooting Installation

- If you receive an error that the package could not install because
  `rlang` or another package could not be upgraded, the easiest way to
  deal with this is to uninstall the package(s) that could not be
  updated (`remove.packages("rlang")`). Then restart R, re-install with
  `install.packages("rlang")` and then try installing `biometryassist`
  again.
