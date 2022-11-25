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
