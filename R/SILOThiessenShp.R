#' Function to generate Thiessen polygons from SILO sites
#' 
#'@param SILOdata - data loaded from SILO based on site list
#'@param  path - file path to save Thiessen polygon shapefile
#'@param  shpname - filename to save ESRI shapefile (no extension)
#'@param boundary - optional filename(including path) of a boundary, e.g. catchment boundary, to apply.
#'
#'@return A simple feature geometry (sf::sfc object) of the polgyons created. Shape file saved to path \\ shpname
#'
#' If boundary is specified weights are written to the attribute table of the polygon return, which can be extracted with
#' \code{st_drop_geometry(returnedfeature[c("Station","weights")])}
#'
#'@examples  
#' \dontrun{
#'X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#'p<-SILOThiessenShp(X,tempdir(),"Theissens")
#'a<-SILOSiteSummary(X)
#'ggplot(p)+geom_sf(aes(fill=AnnualRainfall))+
#'geom_point(data=a,aes(Longitude,Latitude))+
#'geom_text(data=a,aes(Longitude,Latitude,label=Site),nudge_y = 0.02)
#'}
#'@export

SILOThiessenShp<- function(SILOdata,path,shpname,boundary=NULL){
  SiteTable <- SILOSiteSummary(SILOdata) #table summarising SILO sites
  SILOLocs <- sf::st_as_sf(SiteTable, coords = c("Longitude","Latitude"))
  
  if(!is.null(boundary))
  {
    area<-sf::st_read(boundary,stringsAsFactors=FALSE)
    area<-area$geometry
  }else
  {
    area<-sf::st_polygon() #empty polygon used for boundary of Thiessen
  }
  
  . <- NULL #address global variable . warning. bit of a hack.
  
  # compute Voronoi (Thiessen) polygons
  TPoly <- SILOLocs %>% 
    sf::st_geometry() %>% 
    do.call(c, .) %>% 
    sf::st_voronoi(area) %>% 
    sf::st_collection_extract()
  
  # match polygons to points for combining with site data table
  SILOLocs$polys <- sf::st_intersects(SILOLocs, TPoly) %>% unlist %>% TPoly[.]
  TPoly_id <- sf::st_set_geometry(SILOLocs, "polys")#["Station"] #Attaching station ID to polygons for export
  
  TPolyCRS <- sf::st_set_crs(TPoly_id, "+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs") #Defining projection - GDA94
  
  if(!is.null(boundary))
  {
    area<-sf::st_transform(area,sf::st_crs(TPolyCRS))
    TpolyCRS<-sf::st_intersection(TPolyCRS,area)
    TpolyCRS$AlbersArea<-sf::st_area(TpolyCRS)
    TpolyCRS$weights<-as.numeric(TpolyCRS$AlbersArea/sum(TpolyCRS$AlbersArea))
  }
  
  return(TpolyCRS)
}
  


