# Use biometryassist analysis templates

`use_template()` copies a pre-built analysis template from the
biometryassist package to your working directory and optionally opens it
for editing. These templates provide standardized approaches for common
agronomic analyses.

## Usage

``` r
use_template(
  template_name = "mixed_model_template.R",
  dest_dir = ".",
  open = TRUE,
  overwrite = FALSE,
  output_name = NULL
)
```

## Arguments

- template_name:

  Name or path of the template file to use. Default is
  `"mixed_model_template.R"`. Available templates can be listed with
  [`list_templates()`](https://biometryhub.github.io/biometryassist/reference/list_templates.md).

- dest_dir:

  Directory where the template should be copied. Default is the current
  working directory (`"."`).

- open:

  Logical (default `TRUE`). Should the template file be opened in the
  default editor after copying?

- overwrite:

  Logical (default `FALSE`). Should existing files be overwritten?

- output_name:

  `character`, Optional. Name for the copied file in the destination
  directory. If not specified, defaults to `"analysis_script.R"`. If
  specified, the template will be copied and renamed to this file. (The
  original template file is not overwritten.)

## Value

The file path to the copied template (invisibly). Called primarily for
its side effects of copying and optionally opening the template file.

## Details

This function is designed to help users get started with biometryassist
analyses by providing tested, documented templates. The templates
include:

- Suggested package loading

- Commented code explaining steps

- Example data exploration

- Common analysis workflows

If a file with the same name already exists in the destination
directory, the function will not overwrite it unless `overwrite = TRUE`
is specified. In this case, it will still open the existing file if
`open = TRUE`.

## See also

- [`list_templates()`](https://biometryhub.github.io/biometryassist/reference/list_templates.md)
  to see available templates

## Examples

``` r
if (FALSE) { # \dontrun{
# Copy and open the default analysis template
use_template()

# Copy a specific template without opening
use_template("anova_template.R", open = FALSE)

# Copy to a specific directory
use_template("mixed_model_template.R", dest_dir = "analyses")

# Overwrite an existing file
use_template("mixed_model_template.R", overwrite = TRUE)
} # }
```
