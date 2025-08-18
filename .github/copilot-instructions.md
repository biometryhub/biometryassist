# biometryassist R Package

The biometryassist package is an R package for designing and analyzing agronomic-style experiments. It provides functions to aid in experimental design and statistical analysis, especially for teaching these concepts. The package is available on CRAN and has comprehensive testing and documentation.

Always reference these instructions first and fallback to search or bash commands only when you encounter unexpected information that does not match the info here.

## Working Effectively

- Install R and system dependencies:
  - `sudo apt-get update && sudo apt-get install -y r-base r-base-dev`
  - **NEVER CANCEL**: APT installation takes 5-10 minutes on a fresh system. Set timeout to 15+ minutes.
  - Install essential R packages via APT: `sudo apt-get install -y r-cran-ggplot2 r-cran-curl r-cran-rlang r-cran-testthat r-cran-lattice r-cran-scales r-cran-stringi r-cran-xml2 r-cran-knitr r-cran-rmarkdown r-cran-devtools`
  - **NEVER CANCEL**: R package installation via APT takes 5-15 minutes. Set timeout to 20+ minutes.

- Build the package:
  - `R CMD build --no-build-vignettes .` -- takes under 1 minute
  - `R CMD check --no-manual --no-vignettes --no-tests *.tar.gz` -- basic check takes 2-3 minutes without dependencies

- Install dependencies and full testing (when CRAN connectivity works):
  - Install missing dependencies: `sudo R -e "install.packages(c('agricolae', 'askpass', 'cowplot', 'emmeans', 'multcompView', 'pracma'), repos='https://cloud.r-project.org')"`
  - **NEVER CANCEL**: Dependency installation can take 10-20 minutes depending on network. Set timeout to 30+ minutes.
  - Full package check: `R CMD check *.tar.gz`
  - **NEVER CANCEL**: Full R CMD check takes 5-15 minutes with all tests. Set timeout to 20+ minutes.

- Run tests:
  - `R -e "library(testthat); test_dir('tests/testthat')"` -- takes 3-8 minutes for full test suite
  - **NEVER CANCEL**: Complete test suite includes statistical computations and can take 8+ minutes. Set timeout to 15+ minutes.

## Validation

- ALWAYS build and test your changes using `R CMD build` and `R CMD check` before committing.
- ALWAYS run at least a subset of tests relevant to your changes using testthat.
- Test core functionality manually when making changes to key functions:
  - Design functions: `design(type = "crd", treatments = LETTERS[1:4], reps = 3, nrows = 3, ncols = 4, quiet = TRUE)`
  - Analysis functions: `multiple_comparisons()` with sample data
  - Template functions: `use_template(open = FALSE)`
- The CI pipeline (`.github/workflows/R-CMD-check.yaml`) tests on multiple R versions and operating systems. Ensure your changes pass local checks first.
- Check that documentation builds correctly: `R -e "devtools::document()"`

## Common Tasks

### Package Structure
```
biometryassist/
├── DESCRIPTION          # Package metadata and dependencies
├── NAMESPACE           # Exported functions and imports
├── R/                  # R source code
│   ├── design.R        # Experimental design functions
│   ├── mct.R          # Multiple comparisons
│   ├── install_asreml.R # ASReml-R installation helper
│   └── use_template.R  # Analysis templates
├── tests/testthat/     # Test files using testthat framework
├── man/               # Function documentation
├── inst/              # Package data and templates
├── vignettes/         # Package documentation
└── .github/workflows/ # CI/CD pipelines
```

### Key Functions and Their Purpose

**Design Functions:**
- `design()` - Create experimental designs (CRD, RCBD, LSD, Split-plot, etc.)
- `des_info()` - Get information about design parameters
- `autoplot()` - Plot experimental designs

**Analysis Functions:**
- `multiple_comparisons()` - Statistical comparisons with corrections
- `resplot()` - Residual plots for model diagnostics
- `variogram()` - Spatial analysis tools
- `logl_test()` - Likelihood ratio tests

**Utility Functions:**
- `use_template()` - Copy analysis templates to working directory
- `install_asreml()` - Install the commercial ASReml-R package
- `heat_map()` - Create heatmaps for experimental data

### Testing Approach

The package uses testthat for testing with extensive coverage:
- **Unit tests**: Individual function behavior in `tests/testthat/test-*.R`
- **Integration tests**: Cross-function workflows in `tests/testthat/test-all-w2.r`
- **Snapshot tests**: Expected output validation using `expect_snapshot_output()`
- **Visual tests**: Plot validation using `vdiffr::expect_doppelganger()`

When making changes:
1. Run relevant test files: `R -e "testthat::test_file('tests/testthat/test-design.R')"`
2. Update snapshots if output changes: `R -e "testthat::snapshot_review()"`
3. For new functions, add corresponding test files following existing patterns

### Common Issues and Solutions

**Network connectivity issues:**
- If `install.packages()` fails with CRAN connectivity, use APT packages: `sudo apt-get install r-cran-*`
- For development packages not in APT, try alternative repos or install from source

**Build failures:**
- Missing dependencies: Check DESCRIPTION file and install required packages
- Vignette build errors: Use `--no-build-vignettes` flag for R CMD build
- Test failures: Run individual test files to isolate issues

**Documentation:**
- Update function documentation in R source files using roxygen2 comments
- Rebuild documentation: `R -e "devtools::document()"`
- Check documentation: `R -e "devtools::check_man()"`

### Package Dependencies

**Required (Imports):**
- Statistical: `agricolae`, `emmeans`, `multcompView`, `pracma`
- Visualization: `ggplot2`, `cowplot`, `lattice`, `scales`
- Utilities: `curl`, `askpass`, `rlang`, `stringi`, `xml2`

**Optional (Suggests):**
- Testing: `testthat`, `mockery`, `withr`, `vdiffr`
- Documentation: `knitr`, `rmarkdown`, `covr`
- Export: `openxlsx`

**Enhanced packages:**
- `asreml` (commercial license required)
- `lme4`, `nlme`, `sommer` (alternative mixed model packages)

### Development Workflow

1. Make changes to R code in `R/` directory
2. Update tests in `tests/testthat/` as needed
3. Build package: `R CMD build --no-build-vignettes .`
4. Check package: `R CMD check *.tar.gz`
5. Run specific tests: `R -e "testthat::test_file('tests/testthat/test-*.R')"`
6. Document changes: `R -e "devtools::document()"`
7. Commit changes after validation

This package follows standard R package development practices with comprehensive testing and GitHub Actions CI/CD integration.