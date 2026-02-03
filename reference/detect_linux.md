# Detect Linux distro, base OS, and version

Reads /etc/os-release and extracts the minimal information needed to
identify the base OS (e.g. ubuntu, rhel) and major version.

## Usage

``` r
detect_linux()
```

## Value

A list with base OS name and major version
