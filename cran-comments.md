# Resubmission

Minor update to add PKGNAME-package \alias and address CHECK comments related to dependancy

# Test environments

* Local Windows 10, R version 4.1.3 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX, windows Ubuntu
* R-hub Fedora Linux, R-devel, clang, gfortran
* win-builder (devel r85851 and release 4.2.3) x86_64-w64-mingw32

## Github actions

Status: OK
0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## Win-builder devel and release

Staus: OK
One NOTE:

>* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'

## R-hub

0 errors v | 0 warnings v | 3 notes x

checking CRAN incoming feasibility ... [11s] NOTE
  Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'
  
  New submission
  
    X-CRAN-Comment: Archived on 2023-10-31 as check problems were not
  CRAN repository db overrides:
  Package was archived on CRAN
      corrected in time.

> checking for non-standard things in the check directory ... NOTE
  Found the following files/directories:
    ''NULL''

> checking for detritus in the temp directory ... NOTE
  Found the following files/directories:
    'lastMiKTeXException'

## Local Windows 10

one WARNING:

> checking package dependencies ... WARNING
  Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165). 
The warning is only generated on the local windows check, none of the others, also suggesting the package is not orphaned.
