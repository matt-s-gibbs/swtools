# SWTools 1.1.0

* Added [HydstraSiteDetails()] function, to query streamflow station data from states that have a hydstra database, NSW, QLD and VIC

# SWTools 1.0.3

* Fix small typo

# SWTools 1.0.2

* change from ggmap to ggspatial package for SILOMap()

# SWTools 1.0.1

* Very minor update to add @aliases to package documentation

# SWTools 1.0

* 1.0 release to align with [AJWR publication](https://doi.org/10.1080/13241583.2023.2214989)

# SWTools 0.2.5.0

* Add colours to match the before and after periods in SILOCorrectSite

# SWTools 0.2.4.0

* Add buffer parameter to SILOSitesfromPolygon to find SILO sites that might be slightly outside the polygon provided.
* Add SILOWriteFunctionsforSource function to bulk write SILO input for Source based on Thiessen polygons, creating functions to load for each time series and to weight them for each subcatchment, and a table to point each functional unit in each subcatchment to these functions using the Rainfall-Runoff model feature editor.
* Remove duplicate colour from SILOQualityCodes

# SWTools 0.2.3.1

* Fix SILOSitesfromPolygon to handle polygon shape file with a projection other than that used by SILO stations

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
