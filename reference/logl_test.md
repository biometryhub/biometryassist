# Conduct a log-likelihood test for comparing terms in ASReml-R models

Conduct a log-likelihood test for comparing terms in ASReml-R models

## Usage

``` r
logl_test(
  model.obj,
  rand.terms = NULL,
  resid.terms = NULL,
  decimals = 3,
  numeric = FALSE,
  quiet = FALSE
)
```

## Arguments

- model.obj:

  An ASReml-R model object

- rand.terms:

  Character vector of random terms to test. Default is NULL.

- resid.terms:

  Character vector of residual terms to test. Default is NULL.

- decimals:

  Number of decimal places to round p-values. Default is 3.

- numeric:

  Logical. Should p-values be returned as numeric? Default is FALSE
  (formatted).

- quiet:

  Logical. Suppress model update messages and warnings? Default is
  FALSE.

## Value

A data frame of terms and corresponding log-likelihood ratio test
p-values.
