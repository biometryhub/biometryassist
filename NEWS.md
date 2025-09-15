# biometryassist 1.3.1

## Bug Fixes

- Update to ensure compatability with upcoming ggplot2 release (#111)


# biometryassist 1.3.0

## Major changes

- Switched to using S3 methods for `resplot()` to enable easier expansion to different models in future. (#100)
- Introduced a new function `export_design_to_excel()`. An excel file can now be created from a design dataframe, instead of just a graphical plot. (#74)
- Introduced a new comparison interval type in `multiple_comparisons()`. 
The new option `int.type = 'tukey'` will now create comparison intervals using Tukey's distribution rather than a _t_-distribution for a regular confidence interval. 
This has been a point of confusion when intervals don't overlap but share letters. (#66)

## Minor changes

- Model inputs are now checked for potential transformations that haven't been supplied in `multiple_comparisons()`. (#83)
- Enabled a `verbose` option for the `quiet` parameter in `install_asreml()` to give more detailed output when required. (#81)
- Added ability to group multiple resplots and variograms into a single grid via a `onepage` argument. (#73)
- Buffer plots are now implemented within the design data frame, rather than just the plotting logic (#68)

## Bug Fixes

- Fixed a bug introduced in v1.2.2 where arguments weren't passed through properly to asreml. (#98)
- Fixed a bug which broke the messages about deprecated parameters in resplot. (#97)
- Fixed a long-standing bug where logl_test would work if random terms were given in resid. (#7)


# biometryassist 1.2.2

## Major changes

- Switched to using S3 methods to get predictions in `multiple_comparisons()` to enable easier expansion to different models in future. (#92)

## Minor changes

- Added ability to produce column plots from `multiple_comparisons()` (#90)
- Add option in multiple_comparisons() to disable letter comparisons (#85)
- Enable custom colour palettes for design plots (#84)

## Bug Fixes

- Fixed CRAN warnings due to changes in the sommer package (#89)
- Fixed errors in `shapiro.test()` within `resplot()` with too many data points (#87)
- Fixed a bug where `heat_map()` didn't work properly with factor columns (#86)
- Split-plot design dataframe doesn't have the required columns (#82)

# biometryassist 1.2.1

## Bug Fixes

- The new `summary_graph()` function was accidentally left out of the CRAN submission.

# biometryassist 1.2.0

## Major changes

- Introduced the `summary_graph()` (#75) and `heat_map()` (#19) functions 
- Enabled arbitrary `row`, `column`, `block` and `treatment` columns to be provided in the `autoplot.design()` function, to enable more general plotting of designs. (#28)

## Minor changes

- Implemented the ability to plot designs with buffer plots. (#68)
- Changed `install_asreml()` to check if there is a later version before downloading.
- Enabled non-standard evaluation input of column names in `autoplot.design()`.


# biometryassist 1.1.3

## Bug fixes

- Better checking of column names in data provided to `multiple_comparisons()` to prevent breaking. (#53)
- Updated install_asreml() to work with ARM chip macOS devices. (#54)
- Wrote a vignette with instructions for installing ASReml-R. (#55)
- Fixed variogram to enable it to work with dsum models (i.e. MET models). (#61)


# biometryassist 1.1.2

## Bug fixes

- Fixed a bug introduced due to a change in names by `predictmeans()`. (#50)
- Added a check for missing `ar1()` component in `variogram()`. (#49)
- Added an error check for terms provided in the wrong order to `multiple_comparisons()` via `classify`. (#48)
- Checked if model needs the `present` argument passed to `predict.asreml()`. (#41)


# biometryassist 1.1.1

## Minor changes

- `multiple_comparisons()` now accepts power transformations and automatically back-transforms. It gains a new argument `power` to provide the transformation power applied in the model to undo. This enables more general Box-Cox transformations. (#36)
- `multiple_comparisons()` no longer produces an error when the `trans` argument is supplied and `offset` is not. It now produces a warning and sets `offset` to 0 when not provided. (#37)
- Added an option to turn off the start up message and version check. Add `options(biometryassist.check = FALSE)` to your .Rprofile file to disable. Partially fixes #6.
- Enabled new colour-blind friendly palettes (#39)

## Bug fixes

- Updated the required version of rlang (>=1.0.0)
- Fixed a bug that didn't allow labels and the x axis to be rotated independently for `autoplot.mct()` (#35)
- Fixed an issue where treatment columns were not determined correctly in `multiple_comparisons()` (#33)
- Fixed a problem with variograms displaying grey values where `NA`s have been produced. (#24 and #25) 
- Fixed some warnings that would be introduced with the forthcoming version of ggplot2 (#42)


# biometryassist 1.1.0

## Major changes

- `multiple_comparisons()` no longer requires calls to `predict.asreml()` to be passed into the function, as the predicted values are now calculated internally. Additional arguments can be passed to `predict.asreml()` via the `...` argument. (#27)

## Minor changes

- The `order` argument of `multiple_comparisons()` has been deprecated in favour of a new argument `descending`. This takes logical (`TRUE` or `FALSE`) values only, so `default` is no longer possible as it was producing incorrect results. (#8)
- `resplt()` has been deprecated in favour of `resplot()` and will be removed in a future version (#20).
- Warnings about lack of convergence are no longer output in `logl_test()`. (#17)

## Bug fixes

- Aliased levels are printed properly in `multiple_comparisons()` now. (#14)
- R.param and G.param are removed from the `asreml()` call on `resplot()` if not explicitly provided. (#21)
- Fixed a bug where `install_asreml()` would not work on macOS Monterey. (#16)


# biometryassist 1.0.0

## Minor changes

- `mct.out()` has been renamed to `multiple_comparisons()`
- `logl.test()` has been renamed to `logl_test()`
- `des.info()` has been renamed to `des_info()`


## Previous versions

For changes prior to biometryassist 1.0.0 see the BiometryTraining package at https://biometryhub.github.io/BiometryTraining/news/index.html. 
