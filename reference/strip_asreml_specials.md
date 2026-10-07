# Strip ASReml-R special functions from a term label

ASReml-R keeps special-function wrappers in its term labels (e.g.
`at(Year):Prior_crop`, `fa(Site, 2):Variety`, `vm(Genotype, Ainv)`), but
`predict.asreml()` expects the bare factor names in `classify`
(`Year:Prior_crop`). This replaces each wrapper named in `specials` with
its first argument. Only ASReml-R's own functions are listed, so base R
calls such as `log(x)` are left alone. The variance-model functions are
only stripped from random terms, since some share names with base
functions (e.g. [`exp()`](https://rdrr.io/r/base/Log.html),
[`diag()`](https://rdrr.io/r/base/diag.html)) that may appear in the
fixed formula.

## Usage

``` r
strip_asreml_specials(labels, specials = "at")
```

## Arguments

- labels:

  Character vector of term labels.

- specials:

  Character vector of function names to strip. Defaults to `"at"`; see
  `asreml_random_specials` and `asreml_covariate_specials`.

## Value

Character vector of labels with the wrappers removed.
