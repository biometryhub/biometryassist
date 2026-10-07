# Handle deprecated parameters

Internal function to warn about a deprecated parameter: one that is
still a formal of the calling function and still works, but is no longer
recommended. For a parameter that has been removed outright, use
[`handle_removed_param()`](https://biometryhub.github.io/biometryassist/reference/handle_removed_param.md).

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

  Name of the deprecated parameter.

- new_param:

  Name of the replacement parameter, or `NULL` if none.

- custom_msg:

  Optional custom message to append to the warning.

- call_env:

  Environment where to check for the deprecated parameter.

## Value

`NULL`, invisibly; called for its side effect (a warning).
