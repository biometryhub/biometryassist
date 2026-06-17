# Create the folder macOS needs for licensing

ASReml-R uses Reprise licence management which requires the folder
`/Library/Application Support/Reprise/` to exist on macOS Big Sur (11)
and later. This function checks for the folder and attempts to create it
if missing, first without elevated privileges, then via a native macOS
authentication dialog using AppleScript.

## Usage

``` r
create_mac_folder(reprise_path = "/Library/Application Support/Reprise/")
```

## Arguments

- reprise_path:

  Character scalar. Path to the Reprise folder. Defaults to
  `/Library/Application Support/Reprise/`. Exposed as an argument
  primarily to allow testing without elevated privileges.

## Value

Logical `TRUE` if the folder exists or was successfully created. Stops
with an informative error if the folder could not be created and the
user cancelled the authentication dialog.
