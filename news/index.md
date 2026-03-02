# Changelog

## biometryassist 1.4.0

CRAN release: 2026-02-03

### Major changes

- Deprecated
  [`des_info()`](https://biometryhub.github.io/biometryassist/reference/des_info.md).
  This function has been superseded by
  [`design()`](https://biometryhub.github.io/biometryassist/reference/design.md),
  and will be removed in a future version.
- Implement `arcsin` transformation handling in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  ([\#60](https://github.com/biometryhub/biometryassist/issues/60)).
- Demonstrated how to add orientation information to design plots via a
  vignette
  ([\#126](https://github.com/biometryhub/biometryassist/issues/126)).
- Major refactoring of
  [`design()`](https://biometryhub.github.io/biometryassist/reference/design.md)
  in preparation for moving to a new backend later
  ([\#102](https://github.com/biometryhub/biometryassist/issues/102)).
  No user-facing changes.
- Added strip-plot designs
  ([\#134](https://github.com/biometryhub/biometryassist/issues/134)).
- Enabled output of p-value matrix from
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md).
  This required changing the
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  output object to a list, but printing to console and autoplot() still
  work as before
  ([\#22](https://github.com/biometryhub/biometryassist/issues/22)).

### Minor changes

- Add interval type ‘none’ for
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  ([\#125](https://github.com/biometryhub/biometryassist/issues/125)).
- Added a print method for
  [`satab()`](https://biometryhub.github.io/biometryassist/reference/satab.md)
  to reliably get the same output
  ([\#133](https://github.com/biometryhub/biometryassist/issues/133)).
- Switched from cowplot to patchwork to speed up resplot (and variogram)
  ([\#29](https://github.com/biometryhub/biometryassist/issues/29)).
- Added and refactored numerous tests throughout to achieve 100% test
  coverage and speed up tests
  ([\#104](https://github.com/biometryhub/biometryassist/issues/104),
  [\#143](https://github.com/biometryhub/biometryassist/issues/143),
  [\#144](https://github.com/biometryhub/biometryassist/issues/144),
  [\#135](https://github.com/biometryhub/biometryassist/issues/135),
  [\#148](https://github.com/biometryhub/biometryassist/issues/148))
- Added asreml downloads for more operating systems and versions
  ([\#158](https://github.com/biometryhub/biometryassist/issues/158)).

### Bug Fixes

- Fixed a bug where hex colours don’t work with
  [`export_design_to_excel()`](https://biometryhub.github.io/biometryassist/reference/export_design_to_excel.md)
  ([\#124](https://github.com/biometryhub/biometryassist/issues/124)).
- Fixed a bug where buffers weren’t added to the plot if they are added
  after initial design creation
  ([\#123](https://github.com/biometryhub/biometryassist/issues/123)).
- Fixed a bug in
  [`install_asreml()`](https://biometryhub.github.io/biometryassist/reference/install_asreml.md)
  where it threw an error with more than one new version
  ([\#122](https://github.com/biometryhub/biometryassist/issues/122)).
- Fixed a bug where calculate_differences clashed with package “MuMIn”
  ([\#131](https://github.com/biometryhub/biometryassist/issues/131)).

## biometryassist 1.3.3

CRAN release: 2025-09-15

### Bug Fixes

- Fixing test issues identified by CRAN after ggplot2 4.0.0 release

## biometryassist 1.3.2

CRAN release: 2025-09-11

### Bug Fixes

- Updates to tests and histograms for compatibility with upcoming
  ggplot2 release.
- Fixed a bug where standard errors that were too small would produce
  plots with missing upper or lower confidence limits due to rounding
  the SE to zero.
  ([\#119](https://github.com/biometryhub/biometryassist/issues/119))
- Fixed a bug that made rotated axis labels overlap the plot area
  ([\#113](https://github.com/biometryhub/biometryassist/issues/113))

## biometryassist 1.3.1

CRAN release: 2025-07-04

### Bug Fixes

- Update to ensure compatibility with upcoming ggplot2 release
  ([\#111](https://github.com/biometryhub/biometryassist/issues/111))

## biometryassist 1.3.0

CRAN release: 2025-06-11

### Major changes

- Switched to using S3 methods for
  [`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
  to enable easier expansion to different models in future.
  ([\#100](https://github.com/biometryhub/biometryassist/issues/100))
- Introduced a new function
  [`export_design_to_excel()`](https://biometryhub.github.io/biometryassist/reference/export_design_to_excel.md).
  An excel file can now be created from a design dataframe, instead of
  just a graphical plot.
  ([\#74](https://github.com/biometryhub/biometryassist/issues/74))
- Introduced a new comparison interval type in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md).
  The new option `int.type = 'tukey'` will now create comparison
  intervals using Tukey’s distribution rather than a *t*-distribution
  for a regular confidence interval. This has been a point of confusion
  when intervals don’t overlap but share letters.
  ([\#66](https://github.com/biometryhub/biometryassist/issues/66))

### Minor changes

- Model inputs are now checked for potential transformations that
  haven’t been supplied in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md).
  ([\#83](https://github.com/biometryhub/biometryassist/issues/83))
- Enabled a `verbose` option for the `quiet` parameter in
  [`install_asreml()`](https://biometryhub.github.io/biometryassist/reference/install_asreml.md)
  to give more detailed output when required.
  ([\#81](https://github.com/biometryhub/biometryassist/issues/81))
- Added ability to group multiple resplots and variograms into a single
  grid via a `onepage` argument.
  ([\#73](https://github.com/biometryhub/biometryassist/issues/73))
- Buffer plots are now implemented within the design data frame, rather
  than just the plotting logic
  ([\#68](https://github.com/biometryhub/biometryassist/issues/68))

### Bug Fixes

- Fixed a bug introduced in v1.2.2 where arguments weren’t passed
  through properly to asreml.
  ([\#98](https://github.com/biometryhub/biometryassist/issues/98))
- Fixed a bug which broke the messages about deprecated parameters in
  resplot.
  ([\#97](https://github.com/biometryhub/biometryassist/issues/97))
- Fixed a long-standing bug where logl_test would work if random terms
  were given in resid.
  ([\#7](https://github.com/biometryhub/biometryassist/issues/7))

## biometryassist 1.2.2

CRAN release: 2025-04-23

### Major changes

- Switched to using S3 methods to get predictions in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  to enable easier expansion to different models in future.
  ([\#92](https://github.com/biometryhub/biometryassist/issues/92))

### Minor changes

- Added ability to produce column plots from
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  ([\#90](https://github.com/biometryhub/biometryassist/issues/90))
- Add option in multiple_comparisons() to disable letter comparisons
  ([\#85](https://github.com/biometryhub/biometryassist/issues/85))
- Enable custom colour palettes for design plots
  ([\#84](https://github.com/biometryhub/biometryassist/issues/84))

### Bug Fixes

- Fixed CRAN warnings due to changes in the sommer package
  ([\#89](https://github.com/biometryhub/biometryassist/issues/89))
- Fixed errors in
  [`shapiro.test()`](https://rdrr.io/r/stats/shapiro.test.html) within
  [`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
  with too many data points
  ([\#87](https://github.com/biometryhub/biometryassist/issues/87))
- Fixed a bug where
  [`heat_map()`](https://biometryhub.github.io/biometryassist/reference/heat_map.md)
  didn’t work properly with factor columns
  ([\#86](https://github.com/biometryhub/biometryassist/issues/86))
- Split-plot design dataframe doesn’t have the required columns
  ([\#82](https://github.com/biometryhub/biometryassist/issues/82))

## biometryassist 1.2.1

CRAN release: 2024-06-05

### Bug Fixes

- The new
  [`summary_graph()`](https://biometryhub.github.io/biometryassist/reference/summary_graph.md)
  function was accidentally left out of the CRAN submission.

## biometryassist 1.2.0

CRAN release: 2024-05-31

### Major changes

- Introduced the
  [`summary_graph()`](https://biometryhub.github.io/biometryassist/reference/summary_graph.md)
  ([\#75](https://github.com/biometryhub/biometryassist/issues/75)) and
  [`heat_map()`](https://biometryhub.github.io/biometryassist/reference/heat_map.md)
  ([\#19](https://github.com/biometryhub/biometryassist/issues/19))
  functions
- Enabled arbitrary `row`, `column`, `block` and `treatment` columns to
  be provided in the
  [`autoplot.design()`](https://biometryhub.github.io/biometryassist/reference/autoplot.md)
  function, to enable more general plotting of designs.
  ([\#28](https://github.com/biometryhub/biometryassist/issues/28))

### Minor changes

- Implemented the ability to plot designs with buffer plots.
  ([\#68](https://github.com/biometryhub/biometryassist/issues/68))
- Changed
  [`install_asreml()`](https://biometryhub.github.io/biometryassist/reference/install_asreml.md)
  to check if there is a later version before downloading.
- Enabled non-standard evaluation input of column names in
  [`autoplot.design()`](https://biometryhub.github.io/biometryassist/reference/autoplot.md).

## biometryassist 1.1.3

CRAN release: 2023-07-19

### Bug fixes

- Better checking of column names in data provided to
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  to prevent breaking.
  ([\#53](https://github.com/biometryhub/biometryassist/issues/53))
- Updated install_asreml() to work with ARM chip macOS devices.
  ([\#54](https://github.com/biometryhub/biometryassist/issues/54))
- Wrote a vignette with instructions for installing ASReml-R.
  ([\#55](https://github.com/biometryhub/biometryassist/issues/55))
- Fixed variogram to enable it to work with dsum models (i.e. MET
  models).
  ([\#61](https://github.com/biometryhub/biometryassist/issues/61))

## biometryassist 1.1.2

CRAN release: 2022-11-25

### Bug fixes

- Fixed a bug introduced due to a change in names by `predictmeans()`.
  ([\#50](https://github.com/biometryhub/biometryassist/issues/50))
- Added a check for missing `ar1()` component in
  [`variogram()`](https://biometryhub.github.io/biometryassist/reference/variogram.md).
  ([\#49](https://github.com/biometryhub/biometryassist/issues/49))
- Added an error check for terms provided in the wrong order to
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  via `classify`.
  ([\#48](https://github.com/biometryhub/biometryassist/issues/48))
- Checked if model needs the `present` argument passed to
  `predict.asreml()`.
  ([\#41](https://github.com/biometryhub/biometryassist/issues/41))

## biometryassist 1.1.1

CRAN release: 2022-10-27

### Minor changes

- [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  now accepts power transformations and automatically back-transforms.
  It gains a new argument `power` to provide the transformation power
  applied in the model to undo. This enables more general Box-Cox
  transformations.
  ([\#36](https://github.com/biometryhub/biometryassist/issues/36))
- [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  no longer produces an error when the `trans` argument is supplied and
  `offset` is not. It now produces a warning and sets `offset` to 0 when
  not provided.
  ([\#37](https://github.com/biometryhub/biometryassist/issues/37))
- Added an option to turn off the start up message and version check.
  Add `options(biometryassist.check = FALSE)` to your .Rprofile file to
  disable. Partially fixes
  [\#6](https://github.com/biometryhub/biometryassist/issues/6).
- Enabled new colour-blind friendly palettes
  ([\#39](https://github.com/biometryhub/biometryassist/issues/39))

### Bug fixes

- Updated the required version of rlang (\>=1.0.0)
- Fixed a bug that didn’t allow labels and the x axis to be rotated
  independently for
  [`autoplot.mct()`](https://biometryhub.github.io/biometryassist/reference/autoplot.md)
  ([\#35](https://github.com/biometryhub/biometryassist/issues/35))
- Fixed an issue where treatment columns were not determined correctly
  in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  ([\#33](https://github.com/biometryhub/biometryassist/issues/33))
- Fixed a problem with variograms displaying grey values where `NA`s
  have been produced.
  ([\#24](https://github.com/biometryhub/biometryassist/issues/24) and
  [\#25](https://github.com/biometryhub/biometryassist/issues/25))
- Fixed some warnings that would be introduced with the forthcoming
  version of ggplot2
  ([\#42](https://github.com/biometryhub/biometryassist/issues/42))

## biometryassist 1.1.0

CRAN release: 2022-04-14

### Major changes

- [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  no longer requires calls to `predict.asreml()` to be passed into the
  function, as the predicted values are now calculated internally.
  Additional arguments can be passed to `predict.asreml()` via the `...`
  argument.
  ([\#27](https://github.com/biometryhub/biometryassist/issues/27))

### Minor changes

- The `order` argument of
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  has been deprecated in favour of a new argument `descending`. This
  takes logical (`TRUE` or `FALSE`) values only, so `default` is no
  longer possible as it was producing incorrect results.
  ([\#8](https://github.com/biometryhub/biometryassist/issues/8))
- [`resplt()`](https://biometryhub.github.io/biometryassist/reference/biometryassist-deprecated.md)
  has been deprecated in favour of
  [`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
  and will be removed in a future version
  ([\#20](https://github.com/biometryhub/biometryassist/issues/20)).
- Warnings about lack of convergence are no longer output in
  [`logl_test()`](https://biometryhub.github.io/biometryassist/reference/logl_test.md).
  ([\#17](https://github.com/biometryhub/biometryassist/issues/17))

### Bug fixes

- Aliased levels are printed properly in
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
  now. ([\#14](https://github.com/biometryhub/biometryassist/issues/14))
- R.param and G.param are removed from the `asreml()` call on
  [`resplot()`](https://biometryhub.github.io/biometryassist/reference/resplot.md)
  if not explicitly provided.
  ([\#21](https://github.com/biometryhub/biometryassist/issues/21))
- Fixed a bug where
  [`install_asreml()`](https://biometryhub.github.io/biometryassist/reference/install_asreml.md)
  would not work on macOS Monterey.
  ([\#16](https://github.com/biometryhub/biometryassist/issues/16))

## biometryassist 1.0.0

CRAN release: 2022-01-28

### Minor changes

- `mct.out()` has been renamed to
  [`multiple_comparisons()`](https://biometryhub.github.io/biometryassist/reference/multiple_comparisons.md)
- `logl.test()` has been renamed to
  [`logl_test()`](https://biometryhub.github.io/biometryassist/reference/logl_test.md)
- `des.info()` has been renamed to
  [`des_info()`](https://biometryhub.github.io/biometryassist/reference/des_info.md)

### Previous versions

For changes prior to biometryassist 1.0.0 see the BiometryTraining
package at
<https://biometryhub.github.io/BiometryTraining/news/index.html>.
