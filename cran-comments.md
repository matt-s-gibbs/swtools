## Test environments
* Local Windows 10, R version 4.0.3 and 4.1.1 x86_64-w64-mingw32 (64-bit)
* github actions MacOSX R version 4.1.0, x86_64-apple-darwin17.0 (64-bit)
* R-hub Ubuntu Linux 20.04.1 LTS, R-release, GCC.
* win-builder (devel and release)

## R CMD check results

there were no ERRORs, WARNINGS or NOTEs on MacOSX

There were no ERRORs, one WARNING and no NOTEs on Windows 10. The WARNING is:

>Requires orphaned package: 'ggmap'
  
It is [understood ggmap is no longer orphaned](https://community.rstudio.com/t/orphaned-package-on-windows-build/84165), and the warning is only generated on windows platform.

There were no ERRORs, one WARNING and one NOTE on Ubuntu Linux. The NOTE is:

> Possibly mis-spelled words in DESCRIPTION:
    eWater (10:82)
  
>  Found the following (possibly) invalid URLs:
>    URL: https://www.flowmatters.com.au/articles/introducing_veneer.html
      From: man/SWTools.Rd
      Status: Error
      Message: libcurl error code 35:
        	schannel: next InitializeSecurityContext failed: SEC_E_ILLEGAL_MESSAGE (0x80090326) - This error usually occurs when a fatal SSL/TLS alert is received (e.g. handshake failed).
        	
"eWater" is a name and not mis-spelled, and the URL is valid.