# List available biometryassist templates

`list_templates()` returns a character vector of available analysis
templates included with the biometryassist package.

## Usage

``` r
list_templates()
```

## Value

[`character()`](https://rdrr.io/r/base/character.html) Vector of
available template file names.

## See also

[`use_template()`](https://biometryhub.github.io/biometryassist/reference/use_template.md)
to copy and use a template

## Examples

``` r
# See what templates are available
list_templates()
#> [1] "aov_template.R"         "mixed_model_template.R"
```
