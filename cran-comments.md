# Resubmission

This is a new version with two new functions and some bug fixes.

# Test environments

* Local Windows 10, R version 4.1.0 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.1.0, x86_64-apple-darwin17.0 (64-bit)
* R-hub Ubuntu Linux, Fedora Linux and Windows Server
* win-builder (devel and release) x86_64-w64-mingw32

## Github actions MacOSX

Status: OK

## Win-builder devel and release

Status: OK

## R-hub

* Windows Server 2022, R-devel, 64 bit

One NOTE:

>* checking for detritus in the temp directory ... NOTE
Found the following files/directories:
  'lastMiKTeXException'
  
I have run devtools::Check(manual=TRUE) locally to try and reproduce this without success. As it does not turn up on other checks, assuming it is a R-hub Windows server issue.

* Ubuntu Linux 20.04.1 LTS, R-release, GCC

Status: OK

* Fedora Linux, R-devel, clang, gfortran

Status: OK

## Local Windows 10

one WARNING:

> checking package dependencies ... WARNING
  Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165). 
The warning is only generated on the local windows check, none of the others, also suggesting the package is not orphaned.
