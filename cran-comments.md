# Resubmission

Minor update to add PKGNAME-package \alias

# Test environments

* Local Windows 10, R version 4.1.3 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.2.3, x86_64-apple-darwin17.0 (64-bit)
* R-hub Ubuntu Linux 20.04.1 LTS R release
* win-builder (devel 4.3.0 and release 4.2.3) x86_64-w64-mingw32

## Github actions MacOSX

Status: OK
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Win-builder devel and release

Staus: OK
One NOTE:

>* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'

## R-hub

Status: OK

## Local Windows 10

one WARNING:

> checking package dependencies ... WARNING
  Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165). 
The warning is only generated on the local windows check, none of the others, also suggesting the package is not orphaned.
