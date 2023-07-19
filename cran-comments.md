## Submission

This version fixes some minor bugs, and adds a vignette.

## Test environments
* Local Windows 10 install, R 4.3.1
* Ubuntu 22.04 (Virtual Machine), R 4.3.1
* Rhub `check_for_cran()`
* Github Actions:
    - macOS: r-release
    - windows: r-devel, r-release, r-oldrel
    - ubuntu 22.04: r-release, r-oldrel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Packages which this enhances but not available for checking:
    'asreml', 'ARTool', 'sommer'
