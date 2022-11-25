This submission is to fix bugs which would be introduced by version 1.0.8 of predictmeans, to be submitted to CRAN shortly.

## Test environments
* Local Windows 10 install, R 4.2.2
* Ubuntu 22.04 (Virtual Machine), R 4.2.2
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

  
  
