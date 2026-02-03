# Get Predictions for Statistical Models

A generic function to get predictions for statistical models.

## Usage

``` r
get_predictions(model.obj, classify, pred.obj = NULL, ...)

# Default S3 method
get_predictions(model.obj, ...)

# S3 method for class 'asreml'
get_predictions(model.obj, classify, pred.obj = NULL, ...)

# S3 method for class 'lm'
get_predictions(model.obj, classify, ...)

# S3 method for class 'lmerMod'
get_predictions(model.obj, classify, ...)

# S3 method for class 'lmerModLmerTest'
get_predictions(model.obj, classify, ...)

# S3 method for class 'lme'
get_predictions(model.obj, classify, ...)
```

## Arguments

- model.obj:

  A model object. Currently supported model objects are asreml, aov/lm,
  lmerMod/lmerModLmerTest.

- classify:

  Name of predictor variable as a string.

- pred.obj:

  Optional precomputed prediction object.

- ...:

  Additional arguments passed to specific methods.

## Value

A list containing predictions, standard errors, degrees of freedom,
response variable label, and aliased names.
