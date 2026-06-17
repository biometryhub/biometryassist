# Format the aliased-levels note for the comparison print methods

Shared by
[`print.mct()`](https://biometryhub.github.io/biometryassist/reference/print.mct.md),
[`print.pairwise_comparisons()`](https://biometryhub.github.io/biometryassist/reference/pairwise_comparisons.md)
and
[`print.reference_comparisons()`](https://biometryhub.github.io/biometryassist/reference/reference_comparisons.md)
so all three report aliasing identically. Returns `NULL` when nothing
was aliased. The levels are listed when there are few; once there are
more than 6 the list is collapsed to a count (to avoid a large block)
and the user is shown how to retrieve them from the `aliased` attribute.

## Usage

``` r
aliased_note(aliased)
```

## Arguments

- aliased:

  Character vector of aliased level labels (or `NULL`).

## Value

A single-line character string, or `NULL`.
