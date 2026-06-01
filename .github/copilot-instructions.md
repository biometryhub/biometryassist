# Copilot Instructions

This file provides guidance to GitHub Copilot when working in this repository.

## Overview

`biometryassist` is an R package (CRAN-published) providing functions to aid in the design and analysis of agronomic/agricultural experiments. It is a renamed successor to the unmaintained `BiometryTraining` package. Many functions are designed to be approachable for users learning experimental design and mixed-model analysis. The package optionally enhances the commercial `asreml` package but does not require it.

## Common Commands

This is a standard R package. There is no build/lint toolchain beyond R's own. Use `R CMD` and the `devtools`/`testthat` ecosystem.

```r
# Load the package for interactive development (from the package root)
devtools::load_all()

# Regenerate NAMESPACE and man/*.Rd from roxygen2 comments - REQUIRED after
# changing any roxygen block, @export, or @importFrom tag
devtools::document()

# Run the full test suite
devtools::test()

# Run a single test file
testthat::test_file("tests/testthat/test-mct.R")

# Run tests matching a name filter within the testthat harness
devtools::test(filter = "design")   # runs test-design.R

# Full R CMD check (what CI runs; must pass clean for CRAN)
devtools::check()

# Format R code with Air (run from a shell, not the R console).
# CI enforces formatting - code that isn't Air-formatted fails the build.
air format .          # format the whole package in place
air format --check .  # check only (what the format-check CI job runs)

# Build README.md from README.Rmd (never edit README.md directly)
devtools::build_readme()

# Test coverage
covr::package_coverage()
```

## Architecture

The package centers on two workflows: **experimental design generation** and **post-model analysis/visualisation**. Exported functions are listed in `NAMESPACE` (auto-generated - do not hand-edit).

### Design workflow (`design()`)

`R/design.R` -> `design()` is the main entry point. It dispatches by `type` (`crd`, `rcbd`, `lsd`, `split`, `strip`, or `crossed:<base>`) to the underlying `agricolae` design generators, then enriches the result.

- `R/design_helpers.R` - internal helpers (`des_info`, `get_design_info`, etc.) that augment the raw `agricolae` design object with row/column layout.
- `R/satab.R` - builds the skeletal ANOVA table (`satab` object, an S3 class with a `print` method) describing the design's sources of variation.
- `R/create_buffers.R` (`add_buffers`) - adds buffer plots (edge/rows/columns/blocks) to a design layout.
- `R/export_excel.R` (`export_design_to_excel`) - writes a design to a styled workbook.
- `design()` returns a list of class `design` containing `$design` (data frame), `$plot.des` (ggplot/patchwork), `$satab`, and optionally `$seed`. Plotting is done via the `autoplot.design` S3 method.

### Analysis and visualisation workflow

These functions operate on already-fitted model objects (`aov`, `asreml`, `lme`/`nlme`, `lmerMod`/`lme4`, `sommer`, `ARTool`). The package is model-engine-agnostic via S3 dispatch and the `Enhances`/`Suggests` packages.

- `R/mct.R` (`multiple_comparisons`) - Tukey HSD / multiple comparison on predicted means, with letter groupings (`multcompView`), back-transformation support, and confidence/comparison intervals. Returns an `mct` S3 object (with `print` and `autoplot` methods).
- `R/prediction_methods.R` - `get_predictions()` generics that extract predicted means from each supported model class; this is the abstraction layer that lets `multiple_comparisons` work across engines.
- `R/resplot.R` + `R/resplot_methods.R` (`resplot`/`resplt`) - diagnostic residual plots, dispatched per model class.
- `R/heatmap.R` (`heat_map`) - spatial heat maps of trial data/residuals by row/column.
- `R/variogram.r` (`variogram`) - spatial variograms for ASReml-R models (see the two reference PDFs in the repo root: Stefanova 2009, Gilmour 1997).
- `R/summary_graph.R` (`summary_graph`), `R/logltest.R` (`logl_test`) - additional analysis helpers.
- `R/autoplot.R` - S3 `autoplot` methods (`autoplot.design`, `autoplot.mct`) built on ggplot2; `patchwork` composes multi-panel output.

### ASReml-R installation helpers

`R/install_asreml.R` (`install_asreml`, `update_asreml`) downloads and installs the correct ASReml-R binary for the user's OS/R version from a shortlink. ASReml is commercial and not on CRAN, so these are installation conveniences, not dependencies.

### Templates

`inst/templates/` holds analysis script templates (`aov_template.R`, `mixed_model_template.R`). `R/use_template.R` exposes `use_template()`, `list_templates()`, `list_templates` to copy them into a user's working directory. `inst/build_manifest.R` generates `inst/manifest.json` (the template index).

### Cross-cutting

- `R/utils.R` - shared internal utilities.
- `R/biometryassist-package.R` - package doc + `utils::globalVariables()` declaration to suppress R CMD check NOTEs for NSE (non-standard evaluation) column names used in ggplot2/data-frame code.
- `R/biometryassist-deprecated.R` - deprecated function shims.

## Testing Notes

- Uses `testthat` edition 3 with parallel execution enabled (`Config/testthat/edition: 3`, `Config/testthat/parallel: true`).
- Visual/plot regression tests rely on snapshot SVGs under `tests/testthat/_snaps/`.
- Snapshots are split by ggplot2 version (`ggplot2-new` vs `ggplot2-old`) - see `ggplot2_variant()` in `tests/testthat/helper-expectations.R`.
- When a plot's appearance intentionally changes, update snapshots with `testthat::snapshot_review()` / `snapshot_accept()`.
- `tests/testthat/helper-expectations.R` defines custom expectations (for example, `expect_design_output`, `equivalent_ggplot2`) and helpers; reuse these rather than re-asserting structure inline.
- `tests/testthat/data/*.Rdata` are pre-fitted model objects for each supported engine, so tests do not need the (commercial/heavy) modelling packages installed at test time.
- `setup-*.R` / `teardown-*.R` files manage per-suite fixtures (for example, `teardown-install_asreml.R`).

## Code Formatting (Air)

The package is formatted with [Air](https://posit-dev.github.io/air/), Posit's R formatter, configured in `air.toml` (line width 80, 4-space indent).

- Formatting is enforced in CI via `.github/workflows/format-check.yaml` (`air format --check .` on PRs and pushes to `main`). Run `air format .` before committing or the check will fail.
- `.vscode/settings.json` enables format-on-save for R files using the `Posit.air-vscode` extension (recommended in `.vscode/extensions.json`), so edits in VS Code are auto-formatted.
- The scratch/research `.R` scripts in the repo root are excluded from formatting via the `exclude` list in `air.toml`, mirroring their `.Rbuildignore` entries. Add new scratch scripts to both lists.

## Conventions

- Roxygen2 with markdown enabled (`Roxygen: list(markdown = TRUE)`). Always run `devtools::document()` after editing roxygen blocks so `NAMESPACE` and `man/` stay in sync.
- New exported functions need an `@export` tag and `@importFrom` declarations for any external functions used (the package avoids bare `library()`/`require()` and full imports).
- `R/` filenames use mixed casing/extensions (`.R` and `.r`); match the existing file when adding to it.
- Loose `.R` scripts in the repo root (for example, `*SamVariogram.R`, `sommer_vs_asreml*.R`, `asreml_*.R`) are scratch/research files, not package code. They are listed in `.Rbuildignore` and excluded from the build.
- The development branch is `dev`; CRAN releases come from `main`. CI (`.github/workflows/`) runs R-CMD-check, test-coverage, pkgdown, and manifest updates.
