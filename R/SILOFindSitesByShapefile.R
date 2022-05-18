#' Find SILO sites within a polygon
#'
#' @param shpFile location to a shapefile to search within for SILO sites
#' @param ssl See SILODownload, if true if true sets ssl_cipher_list="RC4-SHA" for httr::GET()
#' @param buffer distance in km to buffer the shapefile to look for sites outside the catchment
#' 
#' The buffer distance is approximate for a couple of reasons: the shapefile is projected to match SILO site coordinates,
#' WGS84 and sf::st_buffer does not correctly buffer longitude/latitude data. sdaf
#' Also the input distance in km is converted to degrees using the conversion at the equator of 0.008.
#'
#' @return a table of site information including site numbers found within the polygon
#' @export
#'
#' @examples
#' \dontrun{
#' Sites=SILOSitesfromPolygon("path/to/shapefile.shp")
#' SILODownload(Sites$Number,
#' path=tempdir(),
#' startdate="20180101",enddate="20200101")
#' X<-SILOLoad(Sites$Number,path=tempdir())
#' }
SILOSitesfromPolygon<-function(shpFile,ssl=FALSE,buffer=0)
{
  #find stations within 10,000 km of Alice Springs (15590), i.e. all of them.
  siteToOpen<-"https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=15590&radius=10000&sortby=name"
  
  if(ssl){
    A<-httr::with_config( httr::config("ssl_cipher_list" = "RC4-SHA"),httr::GET(siteToOpen))
  }else
  {
    A<-httr::GET(siteToOpen)
  }
  A<-httr::content(A, as="text")
  
  names<-read.table(text=A,sep="|",nrows=1,strip.white = TRUE)
  #This didn't separate some lines and missed around 1000 sites
  #X<-read.table(text=A,sep="|",skip=1,col.names=names,strip.white = TRUE)
  X<-readr::read_fwf(A,col_positions=readr::fwf_positions(c(1, 8,50,59,68,73,81),
                                                          c(6,48,57,66,71,79,86),
                                                          as.character(names)),skip=1)
  
  p<-sp::SpatialPoints(coords=cbind(X$Longitud,X$Latitude),proj4string=sp::CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",doCheckCRSArgs=FALSE))
  
  area = sf::st_read(shpFile)
  area = sf::st_transform(area,"+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs")
  
  if(buffer>0)
  {
    sf::sf_use_s2(FALSE) #ignores errors if input shapefile is not spherical
    #  area <- sf::st_union(area)
    area <- suppressMessages(suppressWarnings(sf::st_buffer(area,buffer*0.008))) #convert km to degrees at the equator. Could improve this and get latitude from the shp file
  }
  #area<- as(area, 'Spatial')
  area<-sf::as_Spatial(area)
  
  stations<-sp::over(p,area,returnList = FALSE)
  
  stations<-X[!is.na(stations[,1]),]
  return(stations)
}
