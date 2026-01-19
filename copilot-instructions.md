# Copilot Instructions for biometryassist

## Repository Overview

This is an R package called `biometryassist` that provides functions to
assist in the design and analysis of agronomic and agricultural
experiments. The package is on CRAN and is actively maintained.

**Key Information:** - Language: R (requires R \>= 4.1.0) - Package
type: Standard R package with documentation via roxygen2 - License:
MIT - Main purpose: Design and analysis tools for agronomic experiments

## Repository Structure

- `R/` - Main R source code files
- `tests/testthat/` - Test files using testthat framework
- `man/` - Auto-generated documentation (do not edit manually)
- `vignettes/` - Package vignettes
- `inst/` - Installed files
- `.github/workflows/` - CI/CD workflows (R-CMD-check, test coverage)

## Development Setup

### Prerequisites

- R \>= 4.1.0
- Recommended: RStudio or similar R IDE
- System dependencies may be required for some packages (e.g., pandoc
  for vignettes)

### Installing Dependencies

``` r
# Install development dependencies
install.packages("devtools")
devtools::install_deps(dependencies = TRUE)
```

## Building and Testing

### Running Tests

Tests use the testthat framework (edition 3) with parallel execution
enabled:

``` r
# Run all tests
devtools::test()

# Or using testthat directly
testthat::test_dir("tests/testthat")
```

### Building the Package

``` r
# Build and check the package (standard R CMD check)
devtools::check()

# Load the package for development
devtools::load_all()
```

### Code Coverage

``` r
# Generate coverage report
covr::package_coverage()
```

### Documentation

Documentation is generated using roxygen2:

``` r
# Update documentation
devtools::document()
```

## Code Style and Conventions

### General Guidelines

- Follow standard R package development practices
- Use roxygen2 for all function documentation
- Include examples in function documentation where appropriate
- Write informative commit messages

### Documentation Style

- Use roxygen2 markdown format (`@details`, `@param`, `@returns`, etc.)
- Note: Both `@return` and `@returns` are used in the codebase; either
  is acceptable
- Include type information in parameter descriptions
- Provide clear examples that demonstrate function usage
- Document all exported functions thoroughly

### Testing Conventions

- Test files in `tests/testthat/` should be named `test-<module>.R` (or
  `.r` - both extensions are used)
- Use descriptive test names:
  `test_that("description of what is being tested", { ... })`
- Use `expect_*()` functions for assertions
- For plots, use
  [`vdiffr::expect_doppelganger()`](https://vdiffr.r-lib.org/reference/expect_doppelganger.html)
  for visual regression testing
- Use `expect_snapshot_output()` for text output that should remain
  consistent

### Code Organization

- One main function per file when possible
- Helper/utility functions can be in `utils.R`
- Methods for S3 generics should be in separate files (e.g.,
  `autoplot.R`, `prediction_methods.R`)
- Keep related functionality together

### Dependencies

- Main dependencies in `Imports:` section of DESCRIPTION
- Optional dependencies in `Suggests:`
- Enhanced packages (like asreml) in `Enhances:`
- Minimize new dependencies when possible

## Special Considerations

### Package-Specific Notes

- This package enhances the commercial `asreml` package but does not
  require it
- Some functions work with multiple modeling packages (asreml, lme4,
  nlme, sommer, ARTool)
- The package is designed for teaching, so clarity and ease of use are
  priorities
- Many functions return list objects with multiple components (design,
  plot, seed, etc.)

### When Making Changes

- Always run `devtools::check()` before submitting changes
- Update NEWS.md for user-facing changes
- Ensure all tests pass
- Update documentation if function signatures or behavior changes
- Consider backward compatibility - this package is on CRAN

### CI/CD

The repository uses GitHub Actions for: - R CMD check across multiple
platforms (macOS, Windows, Ubuntu) - Multiple R versions (devel,
release, oldrel) - Test coverage reporting via codecov - Package
documentation via pkgdown

## Common Tasks

### Adding a New Function

1.  Create or modify the appropriate R file in `R/`
2.  Add roxygen2 documentation above the function
3.  Export the function with `@export` if it should be user-facing
4.  Run `devtools::document()` to update NAMESPACE and man files
5.  Add tests in `tests/testthat/test-<module>.R`
6.  Run `devtools::check()` to ensure no issues

### Fixing a Bug

1.  Add a test that reproduces the bug (if possible)
2.  Fix the bug in the appropriate R file
3.  Verify the test now passes
4.  Run `devtools::check()` to ensure no regressions
5.  Update NEWS.md with bug fix description

### Updating Documentation

1.  Edit roxygen2 comments in the R source file
2.  Run `devtools::document()` to regenerate man files
3.  Preview with `?function_name` after `devtools::load_all()`
4.  For vignettes, edit the .Rmd file in `vignettes/`

## Resources

- Package documentation: <https://biometryhub.github.io/biometryassist/>
- Issue tracker: <https://github.com/biometryhub/biometryassist/issues>
- CRAN page: <https://cran.r-project.org/package=biometryassist>
