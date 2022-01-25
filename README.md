
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biometryassist

<!-- badges: start -->

[![Project Status: Active: The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/biometryhub/biometryassist/branch/master/graph/badge.svg)](https://app.codecov.io/gh/biometryhub/biometryassist?branch=master)
[![R build
status](https://github.com/biometryhub/biometryassist/workflows/R-CMD-check/badge.svg)](https://github.com/biometryhub/biometryassist/actions)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-3.6.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-1.0.0-orange.svg?style=flat-square)](https://github.com/biometryhub/biometryassist/commits/master)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![Hits](https://hits.seeyoufarm.com/api/count/incr/badge.svg?url=https%3A%2F%2Fgithub.com%2Fbiometryhub%2Fbiometryassist&count_bg=%2379C83D&title_bg=%23555555&icon=&icon_color=%23E7E7E7&title=hits&edge_flat=false)](https://hits.seeyoufarm.com)
<!-- badges: end -->

The goal of biometryassist is to provide functions to aid in the Design
and Analysis of Agronomic-style experiments through easy access to
documentation and helper functions, especially while teaching these
concepts.

*This package is a renamed version of BiometryTraining which is no
longer maintained, but can still be found at
<https://biometryhub.github.io/BiometryTraining/>*

------------------------------------------------------------------------

## Installation

Use the following code to install this package:

``` r
if(!require("remotes")) install.packages("remotes") 
remotes::install_github("biometryhub/biometryassist", upgrade = FALSE)
```

## Using the package

Load the package and start using it with:

``` r
library(biometryassist)
```

If you find this pacakge useful, please cite it! Type
`citation("biometryassist")` on the R console to find out how.

## Troubleshooting Installation

-   If you have the latest version already installed, you will get a
    response like:

<span style="color: red;">Skipping install of ‘biometryassist’ from a
github remote, the SHA1 (e034b603) has not changed since last
install.<br> Use `force = TRUE` to force installation</span>

-   The following warning messages can usually be ignored:

<span style="color: red;">WARNING: Rtools is required to build R
packages, but is not currently installed.</span>

<span style="color: red;">Please download and install Rtools custom from
<https://cran.r-project.org/bin/windows/Rtools/>.</span>

<span style="color: red;">Warning messages:<br> 1: In untar2(tarfile,
files, list, exdir) :<br> skipping pax global extended headers<br> 2: In
untar2(tarfile, files, list, exdir) :<br> skipping pax global extended
headers</span>

-   If you receive an error that the package could not install because
    `rlang` or another package could not be upgraded, the easiest way to
    deal with this is to uninstall the package(s) that could not be
    updated (`remove.packages("rlang")`). Then reinstall with
    `install.packages("rlang")` and then try installing `biometryassist`
    again.
