# Find the manifest entry matching the current system

Looks up the manifest for a build exactly matching the slug built by
[`get_r_os()`](https://biometryhub.github.io/biometryassist/reference/get_r_os.md).
If no exact match is found, falls back to the highest available OS
version that does not exceed the current OS version, for the same OS, R
version, and architecture. This handles cases such as running on a newer
OS than VSNi currently builds for.

## Usage

``` r
find_package(manifest, os_ver, warn = TRUE)
```

## Arguments

- manifest:

  A manifest list as returned by
  [`fetch_manifest()`](https://biometryhub.github.io/biometryassist/reference/fetch_manifest.md).

- os_ver:

  A list as returned by
  [`get_r_os()`](https://biometryhub.github.io/biometryassist/reference/get_r_os.md),
  with elements `os`, `os_ver`, `ver`, `os_major`, and `arm`.

## Value

A single package entry list from `manifest$packages`, or `NULL` if no
suitable match was found.

## Details

Windows has no OS version component and uses exact matching only.
