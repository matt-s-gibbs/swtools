## Test environments
* Local Windows 10, R version 4.0.3 and 4.1.1 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.1.0, x86_64-apple-darwin17.0 (64-bit)
* win-builder (devel)


## R CMD check results

there were no ERRORs, WARNINGS or NOTEs on MacOSX

There were no ERRORs, one WARNING and no NOTEs on Windows 10. The WARNING is:

  Requires orphaned package: 'ggmap'
  
It is understood ggmap is no longer orphaned, and the warning is only generated on windows platforms. [Thread here](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165)
