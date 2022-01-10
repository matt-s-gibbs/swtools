#only GDA94 shapefile, that matches SILO coordinates, tested. sp::over is used to intersect point and shapefile.

shpFile<-"../../SBP/GIS/DraftExtents/First_Draft/Cooper.shp" #shapefile to search
ssl<-FALSE #see SILODownload doc

#find stations within 10,000 km of Alice Springs (15590), i.e. all of them.
siteToOpen<-"https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=near&station=15590&radius=10000&sortby=name"

if(ssl){
  A<-httr::with_config( httr::config("ssl_cipher_list" = "RC4-SHA"),httr::GET(siteToOpen))
}else
{
  A<-httr::GET(siteToOpen)
}
A<-httr::content(A,as="text")

names<-read.table(text=A,sep="|",nrows=1,strip.white = TRUE)
X<-read.table(text=A,sep="|",skip=1,col.names=names,strip.white = TRUE)
p<-sp::SpatialPoints(coords=cbind(X$Longitud,X$Latitude),proj4string=sp::CRS("+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs",doCheckCRSArgs=FALSE))

area = sf::st_read(shpFile)
area<- as(area, 'Spatial')

stations<-sp::over(p,area,returnList = FALSE)

stations<-X[!is.na(stations$OBJECTID),]
#return(stations)