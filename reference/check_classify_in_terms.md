# Check if classify term exists in model terms

Checks if the classify term exists in the model terms, handling
interaction terms specified in any order (e.g., B:A when model has A:B,
or A:C:B when model has A:B:C).

## Usage

``` r
check_classify_in_terms(classify, model_terms)
```

## Arguments

- classify:

  Name of predictor variable as a string

- model_terms:

  Character vector of model term labels

## Value

The classify term as it appears in the model (potentially reordered), or
throws an error if not found
