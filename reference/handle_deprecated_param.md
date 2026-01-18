# Handle deprecated parameters

Simple internal function to warn about deprecated parameters

## Usage

``` r
handle_deprecated_param(
  old_param,
  new_param = NULL,
  custom_msg = NULL,
  call_env = parent.frame()
)
```

## Arguments

- old_param:

  Name of the deprecated parameter

- new_param:

  Name of the replacement parameter or NULL if parameter is being
  removed

- custom_msg:

  Optional custom message to append to the warning

- call_env:

  Environment where to check for the deprecated parameter

## Value

Nothing, called for side effects (warnings)
