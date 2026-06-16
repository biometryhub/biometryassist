
<!-- README.md is generated from README.Rmd. Please edit that file -->

# biometryassist

<!-- badges: start -->

[![Project Status: Active: The project has reached a stable, usable
state and is being actively
developed.](https://www.repostatus.org/badges/latest/active.svg)](https://www.repostatus.org/#active)
[![Codecov test
coverage](https://codecov.io/gh/biometryhub/biometryassist/branch/main/graph/badge.svg)](https://app.codecov.io/gh/biometryhub/biometryassist?branch=main)
[![R build
status](https://github.com/biometryhub/biometryassist/workflows/R-CMD-check/badge.svg)](https://github.com/biometryhub/biometryassist/actions)
[![minimal R
version](https://img.shields.io/badge/R%3E%3D-4.1.0-6666ff.svg)](https://cran.r-project.org/)
[![packageversion](https://img.shields.io/badge/Package%20version-1.5.0-orange.svg?style=flat-square)](https://github.com/biometryhub/biometryassist/commits/main)
[![Licence](https://img.shields.io/github/license/mashape/apistatus.svg)](https://choosealicense.com/licenses/mit/)
[![](https://cranlogs.r-pkg.org/badges/biometryassist)](https://cran.r-project.org/package=biometryassist)
![Badge](https://hitscounter.dev/api/hit?url=https%3A%2F%2Fbiometryhub.github.io%2Fbiometryassist&label=Hits&icon=arrow-down-circle&color=%23198754)
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
if(!require("pak")) install.packages("pak")
pak::pak("biometryhub/biometryassist@dev")

# Alternatively
if(!require("remotes")) install.packages("remotes")
remotes::install_github("biometryhub/biometryassist@dev")
```

## Using the package

Load the package and start using it with:

``` r
library(biometryassist)
```

The package supports two main workflows:

**Experimental design** — generate and visualise trial layouts:

- `design()` — create CRD, RCBD, Latin Square, split-plot, strip-plot,
  and factorial designs, with a plot of the layout and a skeletal ANOVA
  table
- `add_buffers()` — add buffer plots around treatment plots, blocks, or
  the trial perimeter
- `export_design_to_excel()` — write a design to a formatted Excel
  workbook

**Post-model analysis and visualisation** — works with models from
`aov`, `lme4`, `nlme`, `asreml`, `sommer`, `glmmTMB`, `afex`, and more:

- `multiple_comparisons()` — Tukey HSD and other multiple comparison
  tests with letter groupings
- `pairwise_comparisons()` — selected pairwise differences or general
  linear contrasts
- `reference_comparisons()` — compare all levels against a reference
  (Dunnett-style)
- `resplot()` — diagnostic residual plots
- `heat_map()` — spatial heat maps of trial data or residuals
- `variogram()` — spatial variograms for ASReml-R models

The package optionally enhances the commercial
[ASReml-R](https://vsni.co.uk/software/asreml-r) package. If you have a
licence, `install_asreml()` and `update_asreml()` make it easy to
install and keep up to date.

For details on recent changes, see [NEWS.md](NEWS.md).

### Example

``` r
library(biometryassist)

# Generate a Randomised Complete Block Design with 11 treatments and 4 reps
des.out <- design(
    type = "rcbd",
    treatments = LETTERS[1:11],
    reps = 4,
    nrows = 11,
    ncols = 4,
    brows = 11,
    bcols = 1,
    seed = 42,
    quiet = TRUE
)
```

<img src="man/figures/README-example-plot-1.png" alt="" width="100%" />

The `$satab` element gives the skeletal ANOVA table for the design:

``` r
cat(des.out$satab)
#> Source of Variation                     df
#>  =============================================
#>  Block stratum                           3
#>  ---------------------------------------------
#>  treatments                              10
#>  Residual                                30
#>  =============================================
#>  Total                                   43
```

## Troubleshooting Installation

- If you receive an error that the package could not install because
  `rlang` or another package could not be upgraded, the easiest way to
  deal with this is to uninstall the package(s) that could not be
  updated (`remove.packages("rlang")`). Then restart R, re-install with
  `install.packages("rlang")` and then try installing `biometryassist`
  again.

## Citation

If you find this package useful in your work, please cite it. Run the
following in R to get the citation details:

``` r
citation("biometryassist")
```

    #> Warning in citation("biometryassist", ): could not determine year for
    #> 'biometryassist' from package DESCRIPTION file
    #> To cite package 'biometryassist' in publications use:
    #> 
    #>   Nielsen S, Rogers S, Conway A (????). _biometryassist: Functions to
    #>   Assist Design and Analysis of Agronomic Experiments_. R package
    #>   version 1.5.0, <https://biometryhub.github.io/biometryassist/>.
    #> 
    #> A BibTeX entry for LaTeX users is
    #> 
    #>   @Manual{,
    #>     title = {biometryassist: Functions to Assist Design and Analysis of Agronomic Experiments},
    #>     author = {Sharon Nielsen and Sam Rogers and Annie Conway},
    #>     note = {R package version 1.5.0},
    #>     url = {https://biometryhub.github.io/biometryassist/},
    #>   }

## Code of Conduct

Please note that the biometryassist project is released with a
[Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to
this project, you agree to abide by its terms.

## Contributing

Contributions are welcome! Please see [CONTRIBUTING.md](CONTRIBUTING.md)
for guidelines on how to get involved.

## Licence

MIT © University of Adelaide Biometry Hub. See [LICENSE.md](LICENSE.md)
for details.
