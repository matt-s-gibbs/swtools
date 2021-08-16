#' Function to generate Thiessen polygons from SILO sites
#' 
#'@param SILOdata - data loaded from SILO based on site list
#'@param  path - file path to save Thiessen polygon shapefile
#'@param  shpname - filename to save ESRI shapefile (no extension)
#'
#'@return nothing to the environment. Shape file saved to path \\ shpname
#'
#'@examples 
#'X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#'SILOThiessenShp(X,tempdir(),"Theissens")
#'@export

SILOThiessenShp<- function(SILOdata,path,shpname){
  SiteTable <- SILOSiteSummary(SILOdata) #table summarising SILO sites
  SILOLocs <- sf::st_as_sf(SiteTable, coords = c("Longitude","Latitude"))

  . <- NULL #address global variable . warning. bit of a hack.
  # compute Voronoi (Thiessen) polygons
  TPoly <- SILOLocs %>% 
    sf::st_geometry() %>% 
    do.call(c, .) %>% 
    sf::st_voronoi() %>% 
    sf::st_collection_extract()
  
  # match polygons to points for combining with site data table
  SILOLocs$polys <- sf::st_intersects(SILOLocs, TPoly) %>% unlist %>% TPoly[.]
  TPoly_id <- sf::st_set_geometry(SILOLocs, "polys")["Station"] #Attaching station ID to polygons for export
  
  TPolyCRS <- sf::st_set_crs(TPoly_id, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") #Defining projection - GDA94
  
  sf::st_write(TPolyCRS,dsn=path,layer=shpname,driver="ESRI Shapefile",delete_layer = TRUE) #Export shapefile
  
  TPoly_annrain <- sf::st_set_geometry(SILOLocs, "polys")["AnnualRainfall"] #For visual display only
  print(plot(TPoly_annrain)) #Plot Thiessen polygons against annual rainfall totals
  }




