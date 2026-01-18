# Install or update the ASReml-R package

Helper functions for installing or updating the ASReml-R package,
intended to reduce the difficulty of finding the correct version for
your operating system and R version.

## Usage

``` r
install_asreml(
  library = .libPaths()[1],
  quiet = FALSE,
  force = FALSE,
  keep_file = FALSE,
  check_version = TRUE
)

update_asreml(...)
```

## Arguments

- library:

  Library location to install ASReml-R. Uses first option in
  [`.libPaths()`](https://rdrr.io/r/base/libPaths.html) by default.

- quiet:

  Logical or character (default `FALSE`). Controls output verbosity.
  `FALSE` shows normal messages, `TRUE` suppresses messages, `"verbose"`
  shows detailed debugging information.

- force:

  Logical (default `FALSE`). Force ASReml-R to install. Useful for
  upgrading if it is already installed.

- keep_file:

  Should the downloaded asreml package file be kept? Default is `FALSE`.
  `TRUE` downloads to current directory. A file path can also be
  provided to save to another directory. See `Details` for more
  information.

- check_version:

  Logical (default `TRUE`). Should function check if there is a newer
  version of asreml available before attempting to download and install?

- ...:

  other arguments passed to `install_asreml()`

## Value

Silently returns `TRUE` if `asreml` installed successfully or already
present, `FALSE` otherwise. Optionally prints a confirmation message on
success.

## Details

The ASReml-R package file is downloaded from a shortlink, and if
`keep_file` is `TRUE`, the package archive file will be saved in the
current directory. If a valid path is provided in `keep_file`, the file
will be saved to that path, but all directories are assumed to exist and
will not be created. If `keep_file` does not specify an existing, valid
path, an error will be shown after package installation.

## Examples

``` r
if (FALSE) { # \dontrun{
# Example 1: download and install asreml
install_asreml()

# Example 2: install asreml and save file for later
install_asreml(keep_file = TRUE)

# Example 3: install with verbose debugging
install_asreml(quiet = "verbose")
} # }
```
