# SWTools 0.2.3.0
* Added SILOSitesfromPolygon to find sites within a shapefile
* some dependencies removed to reduce number of packages installed (RColorBrewer and ggrepel)

# SWTools 0.2.2.0

* Two functions added: SILOCheckConsistency() and SILOCorrectSite(). 
* SILOCheckConsistency provides homogeneity on plots that are similar to previously calculated by SILODoubleMass() and SILOCumulativeDeviation()
* If a change in a rainfall station is detected using SILOCheckConsistency, SILOCorrectSite() can be used to correct the data
* SILOReport() has been changed to use SILOCheckConsistency() instead of SILODoubleMass() and SILOCumulativeDeviation()
* Updates to SILOSiteSummary to correctly report start and end date, as well as the station elevation.

# SWTools 0.2.1.0

* First release to CRAN
* Functions with limited general value removed from the package, i.e. IOTilePlot() 
* TUFLOW related functions moved to a different package to keep dependencies down, TUFLOWR.
* Hydstra related functions removed, as SA hydstra database is no longer.
