#' Function to bulk create functions for SILO data in Source. 
#' 
#' @param X List of SILO station data, loaded into R using SILOLoad.
#' @param boundary path to a subcatchment shapefile containing the subcatchments in the Source catchment model
#' @param shpColumn column in the shapefile attribute table that corresponds to the catchment numbering. 
#' @param functionsfile filename to create with functions to import into Source
#' @param RRfile filename to create to be imported into the Source Rainfall Runoff feature table
#' @param RainfallDatasourcesFolder Name to use when creating a folder in the Source function editor for the rainfall functions and time series variables
#' @param PETDatasourcesFolder Name to use when creating a folder in the Source function editor for the PET functions and time series variables
#' @param RainfallDatafile Filename of data source loaded in Source for rainfall, in formatting used by Source (e.g. for a file called Rain.csv from a relative folder called TimeseriesData is TimeSeriesData_Rain_csv).
#' @param PETDatafile Filename of data source loaded in Source for PET, in formatting used by Source 
#' @param fus character vector of function unit names in the model.
#'
#' It is assumed that the Source rainfall-runoff scenario was created using the Geographic wizard, using the 'draw network' method (as opposed to DEM based)
#' This allows a raster to be loaded into Source, with an integer in each cell representing the different subcatchments. 
#' \code{boundary} should be the path to a polygon shapefile with these catchment boundaries, and an attribute column that represent the catchment numbers. 
#' Typically this shapefile will be used to generate the raster that Source requires.
#' 
#' This function will create two files:
#' \itemize{
#' \item \code{functionsfile} one to be imported into Source using the Function Import/Export plugin. 
#' The functions in the file that will be imported are a time series function for each SILO site in \code{X} and a function
#' weighting these SILO sites using Thiessen polygon areas for each subcatchment in \code{boundary}
#' \item \code{RRFile} This file points each subcatchment and functional unit to the relevant function created for 
#' rainfall and PET, to be Imported in the Rainfall Runoff feature table (Edit-Rainfall Runoff Models and Import button)
#' }
#' 
#' 
#' @return Nothing to the R environment. Files \code{functionsfile} and \code{RRfile} are created.
#' 
#' @export
#'
#' @examples
#'\dontrun{
#' X<-SILOLoad(sites)
#' shpColumn<-"OBJECTID"
#' functionsfile<-"functions.csv"
#' RRfile<-"RRFile.csv"
#' RainfallDatasourcesFolder<-"Rainfall"
#' PETDatasourcesFolder<-"PET"
#' RainfallDatafile<-"TimeSeriesData_Rain_csv"
#' PETDatafile<-"TimeSeriesData_MWet_csv"
#' fus<-c("regolith","igneous","carbonate","sedimentary")
#' SILOWriteFunctionsforSource(X,boundary,shpColumn,functionsfile,RRfile,
#'                             RainfallDatasourcesFolder,PETDatasourcesFolder,
#'                           RainfallDatafile,PETDatafile,fus)
#'}

SILOWriteFunctionsforSource<-function(X,boundary,shpColumn,functionsfile,RRfile,
                                      RainfallDatasourcesFolder,PETDatasourcesFolder,
                                      RainfallDatafile,PETDatafile,fus)
{
  Allarea<-sf::st_read(boundary,stringsAsFactors=FALSE)
  
  #need to find number of digits for padding
  x<-data.frame(Allarea)[,shpColumn]
  digits<-max(floor(log10(x)) + 1)
  
  cat("Subcatchment,Functional Unit,Input Data-PET,Input Data-Rainfall\n",file=RRfile)
  cat("Functions created by SWTools for Thiessen polygon functions\n",file=functionsfile)
  
  #add all the time series variables
  for(site in names(X))
  {
    cat(paste0("TimeSeriesVariable,$",RainfallDatasourcesFolder,".ts_Rain_",site,",",RainfallDatafile,".",site,",mm.day^-1\n"),file=functionsfile,append=TRUE)
    cat(paste0("TimeSeriesVariable,$",PETDatasourcesFolder,".ts_PET_",site,",",PETDatafile,".",site,",mm.day^-1\n"),file=functionsfile,append=TRUE)
  }
  
  for(i in 1:nrow(Allarea))
  {
    area<-Allarea[i,]
    area<-area$geometry
    
    weights<-SILOThiessenShp(X,
                             "C:\\Users\\GIB417\\OneDrive - CSIRO\\ROWRA\\GregoryNicholsonSource\\SpatialData",
                             "theissen",
                             area)
    subcatID<-formatC(data.frame(Allarea)[i,shpColumn],width=digits,flag="0")
    Rainfallfunctionname<-paste0(RainfallDatasourcesFolder,".f_SC",subcatID)
    PETfunctionname<-paste0(PETDatasourcesFolder,".f_SC",subcatID)
    
    thiesseneqnP<-NULL
    thiesseneqnPET<-NULL
    weights<-data.frame(weights)
    for(w in 1:nrow(weights))
    {
      thiesseneqnP<-paste0(thiesseneqnP,"$ts_Rain_",weights[w,"Station"]," * ",round(weights[w,"weights"],3)," +")
      thiesseneqnPET<-paste0(thiesseneqnPET,"$ts_PET_",weights[w,"Station"]," * ",round(weights[w,"weights"],3)," +")
    }
    #remove the last " +"
    thiesseneqnP<-substr(thiesseneqnP,1,nchar(thiesseneqnP)-2)
    thiesseneqnPET<-substr(thiesseneqnPET,1,nchar(thiesseneqnPET)-2)
    
    
    cat(paste0("Function,$",Rainfallfunctionname,",",thiesseneqnP,",mm.day^-1,0,StartOfTimeStep,False\n"),file=functionsfile,append=TRUE)
    cat(paste0("Function,$",PETfunctionname,",",thiesseneqnPET,",mm.day^-1,0,StartOfTimeStep,False\n"),file=functionsfile,append=TRUE)
    for(f in fus)
    {
      cat(paste0("SC #",subcatID,",",f,",$",PETfunctionname,",$",Rainfallfunctionname,"\n"),file=RRfile,append=TRUE)
    }
    
  }
}
