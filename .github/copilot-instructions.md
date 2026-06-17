# Copilot Instructions

This file provides guidance to GitHub Copilot when working in this repository.

## Overview

`biometryassist` is an R package (CRAN-published) providing functions to aid in the design and analysis of agronomic/agricultural experiments. It is a renamed successor to the unmaintained `BiometryTraining` package. Many functions are designed to be approachable for users learning experimental design and mixed-model analysis. The package optionally enhances the commercial `asreml` package but does not require it.

## Common Commands

Use the standard R package workflow with `devtools` and `testthat`.

```r
# Load the package for interactive development
devtools::load_all()

# Regenerate NAMESPACE and man/*.Rd from roxygen2 comments
devtools::document()

# Run the full test suite
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-mct.R")

# Run tests matching a filter
devtools::test(filter = "design")

# Full R CMD check
devtools::check()

# Build README.md from README.Rmd
devtools::build_readme()

# Coverage report
covr::package_coverage()
```

## Architecture

The package centers on two workflows: experimental design generation and post-model analysis/visualisation. Exported functions are listed in `NAMESPACE`, which is auto-generated and should not be edited by hand.

### Design Workflow

`R/design.R` defines `design()`, the main entry point. It dispatches by `type` (`crd`, `rcbd`, `lsd`, `split`, `strip`, or `crossed:<base>`) to `agricolae` design generators, then enriches the result.

- `R/design_helpers.R` contains internal helpers such as `des_info` and `get_design_info`.
- `R/satab.R` builds the skeletal ANOVA table returned as a `satab` object.
- `R/create_buffers.R` implements `add_buffers()` for edge, row, column, and block buffers.
- `R/export_excel.R` implements `export_design_to_excel()` for workbook export.
- `design()` returns a `design` object containing `$design`, `$plot.des`, `$satab`, and optionally `$seed`.

### Analysis and Visualisation Workflow

These functions operate on fitted model objects such as `aov`, `asreml`, `lme`/`nlme`, `lmerMod`/`lme4`, `sommer`, and `ARTool`.

- `R/mct.R` implements `multiple_comparisons()` for Tukey HSD / predicted means comparisons.
- `R/prediction_methods.R` provides `get_predictions()` methods for supported model classes.
- `R/resplot.R` and `R/resplot_methods.R` implement `resplot()` / `resplt()` diagnostics.
- `R/heatmap.R` implements `heat_map()` for spatial visualisation.
- `R/variogram.r` implements `variogram()` for ASReml-R models.
- `R/summary_graph.R` implements `summary_graph()`.
- `R/logltest.R` implements `logl_test()`.
- `R/autoplot.R` contains `autoplot.design` and `autoplot.mct` methods.

### ASReml-R Installation Helpers

`R/install_asreml.R` provides `install_asreml()` and `update_asreml()` helpers for downloading the correct ASReml-R binary for the user's OS and R version. ASReml is commercial and not a package dependency.

### Templates

`inst/templates/` contains analysis script templates such as `aov_template.R` and `mixed_model_template.R`. `R/use_template.R` exposes `use_template()` and `list_templates()` to copy them into the user's working directory. `inst/build_manifest.R` generates `inst/manifest.json`.

### Cross-cutting

- `R/utils.R` contains shared internal utilities.
- `R/biometryassist-package.R` contains package documentation and `utils::globalVariables()` declarations.
- `R/biometryassist-deprecated.R` contains deprecated function shims.

## Testing Notes

- The package uses `testthat` edition 3 with parallel execution enabled.
- Plot regression tests rely on SVG snapshots under `tests/testthat/_snaps/`.
- Snapshot folders are split by ggplot2 version (`ggplot2-new` vs `ggplot2-old`).
- Use the helpers in `tests/testthat/helper-expectations.R` such as `expect_design_output` and `equivalent_ggplot2` when possible.
- `tests/testthat/data/*.Rdata` contains pre-fitted model objects for supported engines, avoiding heavy runtime dependencies in tests.
- `setup-*.R` and `teardown-*.R` files manage per-suite fixtures.

## Code Formatting

The package is formatted with [Air](https://posit-dev.github.io/air/), configured in `air.toml` with a line width of 80 and 4-space indent.

- Run `air format .` before committing changes.
- CI enforces formatting with `air format --check .`.
- `.vscode/settings.json` enables format-on-save for R files using `Posit.air-vscode`.
- Scratch and research scripts in the repository root are excluded via `air.toml` and `.Rbuildignore`.

## Conventions

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). Always run `devtools::document()` after editing roxygen blocks so `NAMESPACE` and `man/` stay in sync.
- New exported functions need an `@export` tag and `@importFrom` declarations for any external functions used (the package avoids bare `library()`/`require()` and full imports).
- `R/` filenames use mixed casing/extensions (`.R` and `.r`); match the existing file when adding to it.
- Loose `.R` scripts in the repo root (for example, `*SamVariogram.R`, `sommer_vs_asreml*.R`, `asreml_*.R`) are scratch/research files, not package code. They are listed in `.Rbuildignore` and excluded from the build.
- The development branch is `dev`; CRAN releases come from `main`. CI (`.github/workflows/`) runs R-CMD-check, test-coverage, pkgdown, and manifest updates.
