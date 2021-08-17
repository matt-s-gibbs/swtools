#' Run Source using Veneer
#' 
#' @param StartDate Optional. Start date for simulation. Must be dd/mm/yyyy
#' @param EndDate Optional. End date for simulation. Must be dd/mm/yyyy
#' @param InputSet Optional. Input set to use
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return Nothing to the R environment.
#' 
#' If not set, the configuration parameters (StartDate, EndDate, InputSet), was is specified
#' in the Source configuration in the GUI will be used.
#' 
#' The console will show any errors returned by Veneer. 
#' 
#' @examples 
#' \dontrun{
#' VeneerRunSource()
#' VeneerRunSource("01/07/2017","01/02/2018","NoDams")
#' }
#' 
#'@export

VeneerRunSource<-function(StartDate=NULL,EndDate=NULL,InputSet=NULL,baseURL="http://localhost:9876")
{
  X<-list()
  if(!is.null(StartDate)) X[["StartDate"]]<-StartDate
  if(!is.null(EndDate)) X[["EndDate"]]<-EndDate
  if(!is.null(InputSet)) X[["SelectedInputSet"]]<-InputSet
  
  X<-jsonlite::toJSON(X,auto_unbox = TRUE)
  A<-httr::POST(paste0(baseURL,"/runs"),body=X,httr::content_type_json())
  
  #write error message to console if there was one
  if(substr(rawToChar(A[[6]]),3,9)=="Message"){
    return(substr(strsplit(rawToChar(A[[6]]),",")[[1]][1],13,1000))
  }else{
    return("Run Successful")
  }
    
}

#'  Update a function value or expression. Function must exist before being updated.
#' 
#' @param Name Name of the function without the "$", e.g. f_ScaleFactor
#' @param Expression Expression to change it to, e.g. 1.2
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return Nothing to the R environment.
#' 
#' @examples 
#' \dontrun{
#'  VeneerSetFunction("f_ScaleFactor",1.2)
#'  VeneerSetFunction("f_TargetLevel","if($m_Flow<1000,3.2,3.5)")
#'  }
#'  
#'@export

VeneerSetFunction<-function(Name,Expression,baseURL="http://localhost:9876")
{
  X<-list("Expression"=as.character(Expression),"Name"=paste0("$",Name))
  X<-jsonlite::toJSON(X,auto_unbox = TRUE)

  httr::PUT(paste0(baseURL,"/functions/",Name),body=X,httr::content_type_json())
}

#' Change a Source piecewise table using Veneer
#' 
#' @param data A 2 column data.frame or matrix with the data to load into the piecewise table.
#' @param pw_table The name of the piecewise linear variable, without the "$".
#' @param baseURL URL of the Veneer server. Defaults to the veneer default. 
#' 
#' @return Nothing to the R environment.
#' 
#' @examples 
#' \dontrun{
#'  data<-data.frame(X=seq(1,5),Y=seq(1,5))
#'  VeneerSetPiecewise(data,"pw_table")
#' }
#'  
#' @export

VeneerSetPiecewise<-function(data,pw_table,baseURL="http://localhost:9876")
{
  if(ncol(data)!=2)
  {
    stop("Data for piecewise linear must have 2 columns")
  }
  X<-list()
  X[["Entries"]]<-as.matrix(data)
  X[["XName"]]<-"Lookup"
  X[["YName"]]<-"Result"
  
  X<-jsonlite::toJSON(X,auto_unbox = TRUE)
  
  #Name in here, or not??
  httr::PUT(paste0(baseURL,"/variables/",pw_table,"/Piecewise"),body=X,httr::content_type_json())
}

#' Get data from a Source piecewise table using Veneer
#' 
#' @param pw_table The name of the piecewise linear variable, without the $
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return a matrix with the data from the piecewise table.
#' 
#' 
#'  
#' @examples 
#' \dontrun{
#' VeneerGetPiecewise(data,"pw_table")
#' }
#'  
#'  
#' @export
#' @importFrom utils URLencode

VeneerGetPiecewise<-function(pw_table,baseURL="http://localhost:9876")
{
  
  #Name in here, or not??
  D<-jsonlite::fromJSON(URLencode(paste0(baseURL,"/variables/",pw_table,"/Piecewise")))$Entries
  return(D)
}

#'Get a time series result from Source using Veneer
#' @param TSURL, the URL of the time series to retrieve
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return a zoo time series of the data
#' 
#' The URL of the time series must be specified, by interrogation using a browser or other analysis. 
#' By default Source returns SI units. Some conversion is undertaken:
#' * Flow converted to ML/d
#' * Volume converted to ML
#' * Area converted to ha
#' 
#' Spaces are OK, like in the example below (dont need to insert %20 for example).
#' 
#' @examples 
#' \dontrun{
#' VeneerGetTS("/runs/latest/location/EndofSystem/element/Downstream Flow/variable/Flow")
#' }
#' 
#' @export
#' @importFrom utils URLencode

VeneerGetTS<-function(TSURL,baseURL="http://localhost:9876")
{
  D<-jsonlite::fromJSON(URLencode(paste0(baseURL,TSURL)))
  B<-zoo::zoo(D$Events$Value,zoo::as.Date(D$Events$Date,format="%m/%d/%Y"))

  if(D$Units=="m\U00B3/s") B <- B*86.4 #m3/s to ML/d
  if(D$Units == "m\U00B3") B <- B / 1000 #m3 to ML
  if(D$Units == "m\U00B2") B <- B / 10000 #m2 to ha
  if(D$Units == "kg/m\U00B3") B <- B * 1000 #kg/m³ to mg/L
  
  return(B)
}

#' Get all time series recorded in Source of a given variable type
#' @param variable Which variable to retrieve. Defaults to Flow.
#' @param run Which run to retrieve from. Defaults to the latest
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return a zoo time series, with each output as a column
#' 
#' @examples 
#' \dontrun{
#' VeneerGetTSbyVariable() #returns all flow outputs recorded in the latest run
#' VeneerGetTSbyVariable("Water Surface Elevation",1) 
#' }
#' 
#' @export
#' 
VeneerGetTSbyVariable<-function(variable="Flow",run="latest",baseURL="http://localhost:9876")
{
  Results<-jsonlite::fromJSON(paste0(baseURL,"/runs/",run))
  X<-Results$Results %>% dplyr::filter(.data$RecordingVariable==variable)
  TS<-lapply(X$TimeSeriesUrl,function(x) VeneerGetTS(x,baseURL))
  if(length(TS)>0)
  {
    TS<-zoo::zoo(matrix(unlist(TS),ncol=length(TS)),zoo::index(TS[[1]]))
    if(ncol(TS)>1){
      colnames(TS)<-X$NetworkElement
    }else
    {
      names(TS)<-X$NetworkElement
    }
    return(TS)
  }else
  {
    stop(paste("No results for variable",variable,"found for run",run,"\n",
      "Recorded variables are:",paste(unique(Results$Results$RecordingVariable))))
  }
}

#' Get a vector of the type of time series variables recorded
#' @param run Which run to retrieve from. Defaults to the latest
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return a vector of variable types (e.g. Downstream flow, Downstream Flow Concentration, water surface elevation)
#' 
#' @examples
#' \dontrun{
#'  VeneerGetTSVariables()
#' }
#' 
#' @export
#' 
VeneerGetTSVariables<-function(run="latest",baseURL="http://localhost:9876")
{
  Results<-jsonlite::fromJSON(paste0(baseURL,"/runs/",run))
  return(unique(Results$Results$RecordingVariable))
}

#' Get all time series recorded in Source for a given node
#' @param Node Name of node to retrieve Time Series for
#' @param run Which run to retrieve from. Defaults to the latest
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return a zoo time series, with each variable as a column
#' 
#' @examples 
#' \dontrun{
#' VeneerGetTSbyNode("Storage 1")
#' }
#' 
#' @export

VeneerGetTSbyNode<-function(Node,run="latest",baseURL="http://localhost:9876")
{
  Results<-jsonlite::fromJSON(paste0(baseURL,"/runs/",run))
  X<-Results$Results %>% dplyr::filter(.data$NetworkElement==Node)
  TS<-lapply(X$TimeSeriesUrl,function(x) VeneerGetTS(x,baseURL))
  if(length(TS)>0)
  {
    TS<-zoo::zoo(matrix(unlist(TS),ncol=length(TS)),zoo::index(TS[[1]]))
    if(ncol(TS)>1) colnames(TS)<-X$RecordingVariable
    return(TS)
  }else
  {
    stop(paste("No results for node",Node,"found for run",run,"\n",
      "Recorded Nodes are:",paste(unique(Results$Results$NetworkElement))))
  }
}

#' Get vector of InputSets
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return vector containing info on Input Sets in the model
#' 
#' @examples 
#' \dontrun{
#' VeneerGetInputSets()
#' }
#' 
#' @export

VeneerGetInputSets<-function(baseURL="http://localhost:9876")
{
  return(jsonlite::fromJSON(paste0(baseURL,"/InputSets")))
}

#' Get a vector of node names for a given type
#' @param NodeType The node to return the names of. The icon in /network is searched for this name
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#'@return vector of node names matching the specified node type
#'
#'@examples 
#'\dontrun{
#'VeneerGetNodesbyType("Weir")
#'}
#'
#'@export

VeneerGetNodesbyType<-function(NodeType,baseURL="http://localhost:9876")
{
  A<-jsonlite::fromJSON(paste0(baseURL,"/network"))
  
  #find the name of the nodetype
  iconname<-A$features$properties$icon[grep(NodeType,A$features$properties$icon)[1]]
  if(length(iconname)==1 & is.na(iconname)){
    stop(paste(NodeType,"not found in the model. Try a different name, capitalisation matters. Search http://localhost:9876/network to see options, look for \"icon\""))
  }else{
    return(A$features$properties %>% dplyr::filter(.data$icon == iconname) %>% dplyr::select(.data$name))
  }
}

#' Get the number of the latest run
#' @param baseURL URL of the Veneer server. Defaults to the veneer default.
#' 
#' @return integer of the latest run number
#' 
#' @examples 
#' \dontrun{
#' VeneerlatestRunNumber()
#' }
#' 
#' @export

VeneerlatestRunNumber<-function(baseURL="http://localhost:9876")
{
  A<-jsonlite::fromJSON(paste0(baseURL,"/Runs"))
  return(as.integer(strsplit(A[nrow(A),]$RunUrl,"/")[[1]][3]))
}