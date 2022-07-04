# Resubmission

This is a minor update to functionality.

# Test environments

* Local Windows 10, R version 4.1.0 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.1.0, x86_64-apple-darwin17.0 (64-bit)
* R-hub solaris-x86-patched
* win-builder (devel and release) x86_64-w64-mingw32

## Github actions MacOSX

Status: OK

## Win-builder devel and release

One NOTE:

>* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'

## R-hub

* solaris-x86-patched

Status: OK

## Local Windows 10

one WARNING:

> checking package dependencies ... WARNING
  Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165). 
The warning is only generated on the local windows check, none of the others, also suggesting the package is not orphaned.
