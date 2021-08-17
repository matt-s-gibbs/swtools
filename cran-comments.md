## Resubmission
This is a re-submission. In this version I have:

* Added value tags to all function documentation. 
* Converted print() statements used for error checking to warning() or stop() depending if the error is fatal.
* Configured aspell to prevent spelling errors notes in DESCRIPTION

Thankyou for the quick review and helpful comments.

## Test environments
* Local Windows 10, R version 4.1.1 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.1.0, x86_64-apple-darwin17.0 (64-bit)
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC.
* win-builder (devel and release) x86_64-w64-mingw32

## R CMD check results

### Github actions MacOSX
No ERRORs, WARNINGS or NOTEs

### Win-builder
No ERRORs, WARNINGS or NOTEs

### R-hub Ubuntu linux
No ERRORs, WARNINGS or NOTEs

### Local Windows 10
one WARNING:

> checking package dependencies ... WARNING
  Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165). 
The warning is only generated on the local windows check, none of the others, also suggesting the package is not orphaned.