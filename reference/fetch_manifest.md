# Fetch the ASReml build manifest

Downloads and parses the JSON manifest used to decide which ASReml build
(URL + version) matches the current OS and R version.

## Usage

``` r
fetch_manifest(
  manifest_url =
    "https://raw.githubusercontent.com/biometryhub/biometryassist/main/inst/manifest.json"
)
```

## Arguments

- manifest_url:

  Character scalar. URL to a JSON manifest in the same structure as the
  package's `inst/manifest.json`.

## Value

A list as returned by
[`jsonlite::fromJSON()`](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)
with at least a `packages` element (a list of package entries).
