## Test environments
* Local Windows 10 install, R 4.1.3
* Ubuntu 20.04 (on Virtual Box), R 4.1.3
* Rhub `check_for_cran()`
* Github Actions:
    - macOS: r-release
    - windows: r-devel, r-release, r-oldrel
    - ubuntu 20.04: r-release, r-oldrel

## R CMD check results
There were no ERRORs or WARNINGs. 

There was 1 NOTE:

* checking package dependencies ... NOTE
  Packages which this enhances but not available for checking:
    'asreml', 'sommer'
  
  
