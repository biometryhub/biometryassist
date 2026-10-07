# Handle removed parameters

Internal function to stop on a parameter that has been removed. A
removed parameter is no longer a formal of the calling function, so it
is looked for in the caller's `...`, where it would otherwise be
silently captured and passed on. For a parameter that still works but is
no longer recommended, use
[`handle_deprecated_param()`](https://biometryhub.github.io/biometryassist/reference/handle_deprecated_param.md).

## Usage

``` r
handle_removed_param(
  old_param,
  new_param = NULL,
  custom_msg = NULL,
  version = NULL,
  call_env = parent.frame()
)
```

## Arguments

- old_param:

  Name of the removed parameter.

- new_param:

  Name of the replacement parameter, or `NULL` if none.

- custom_msg:

  Optional custom message to append to the error.

- version:

  Optional version in which the parameter was removed, e.g. `"1.5.0"`,
  so users can find the change in NEWS.

- call_env:

  Environment of the calling function, whose `...` is checked.

## Value

`NULL`, invisibly; called for its check.
