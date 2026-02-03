# Detect operating system and construct OS/version key

Determines OS, OS version (if applicable), architecture, and combines
this with the R version for downstream download logic.

## Usage

``` r
get_r_os()
```

## Value

A list with os_ver, os, ver, and arm
