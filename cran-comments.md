# Resubmission

Fix typo

# Test environments

* Local Windows 10, R version 4.1.3 x86_64-w64-mingw32 (64-bit)
* github actions MacOS, windows, ubuntu
* win-builder (devel r85851 and release 4.2.3) x86_64-w64-mingw32

## Github actions

Status: OK
One NOTE:
>Namespace in Imports field not imported from: ‘prettymapr’
  All declared Imports should be used.
  
prettymapr is used via a ggspatial function, and is not called directly. 

## Win-builder devel and release

Staus: OK
One NOTE:

>* checking CRAN incoming feasibility ... NOTE
Maintainer: 'Matt Gibbs <gibbs.ms@gmail.com>'

## Local Windows 10

0 errors ✔ | 0 warnings ✔ | 0 notes ✔
