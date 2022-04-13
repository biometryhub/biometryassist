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
