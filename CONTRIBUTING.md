# Contributing to biometryassist

Thanks for your interest in contributing to biometryassist! Contributions of all kinds are welcome — bug reports, feature requests, documentation improvements, and code.

By participating in this project, you agree to abide by our [Code of Conduct](CODE_OF_CONDUCT.md).

## Reporting bugs and requesting features

Please file bug reports and feature requests as [GitHub issues](https://github.com/biometryhub/biometryassist/issues).

For bug reports, please include a minimal [reproducible example](https://reprex.tidyverse.org/) (a "reprex") that demonstrates the problem, along with the output of `sessionInfo()`. A good reprex makes it much easier for us to diagnose and fix the issue.

Before opening a new issue, please search the existing issues to check it hasn't already been reported.

## Contributing code

We use the standard [GitHub fork and pull request workflow](https://docs.github.com/en/get-started/quickstart/contributing-to-projects):

1. Fork the repository and create a branch from `dev` (this is the development branch; `main` holds the CRAN release).
2. Make your changes, following the conventions below.
3. Push to your fork and open a pull request against the `dev` branch.

For substantial changes, please open an issue first to discuss what you would like to change. This helps avoid duplicated effort and ensures the change fits the direction of the package.

### Development setup

This is a standard R package developed with the `devtools`/`testthat` ecosystem:

```r
# Load the package for interactive development
devtools::load_all()

# Regenerate NAMESPACE and man/*.Rd after editing roxygen comments
devtools::document()

# Run the test suite
devtools::test()

# Full R CMD check (must pass clean)
devtools::check()
```

### Conventions

- **Documentation**: We use [roxygen2](https://roxygen2.r-lib.org/) with markdown enabled. Always run `devtools::document()` after editing any roxygen block so that `NAMESPACE` and `man/` stay in sync. New exported functions need an `@export` tag and `@importFrom` declarations for any external functions used.
- **Code formatting**: Code is formatted with [Air](https://posit-dev.github.io/air/) (line width 80, 4-space indent), configured in `air.toml`. Run `air format .` before committing — formatting is enforced in CI, so unformatted code will fail the build. If you use VS Code, format-on-save is enabled via the recommended Air extension.
- **Tests**: Please add or update tests for any new functionality or bug fixes. We use `testthat` (edition 3). Some tests use snapshot/visual regression — if you change plotting output, the maintainers will review the snapshot changes before accepting them.
- **NEWS**: Please add a bullet to `NEWS.md` describing user-facing changes.

### Pull request checklist

Before submitting, please make sure that:

- [ ] `devtools::document()` has been run and the resulting changes are committed
- [ ] `air format .` has been run
- [ ] `devtools::check()` passes with no errors, warnings, or new notes
- [ ] Tests have been added or updated as appropriate
- [ ] `NEWS.md` has been updated for user-facing changes

## Questions

If you have a question that isn't a bug report or feature request, feel free to open an issue or contact the maintainer at <biometrytraining@adelaide.edu.au>.

## Code of Conduct

Please note that this project is released with a [Contributor Code of Conduct](CODE_OF_CONDUCT.md). By contributing to this project, you agree to abide by its terms.
