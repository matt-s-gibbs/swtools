#' Function to load in an Aquarius json file, downloaded from https://water.data.sa.gov.au, possibly using AWQPDownload()
#'
#'@param filename A file downloaded from the Export Data tab on https://water.data.sa.gov.au, or using AQWPDownload()
#'@param qual_codes TRUE/FALSE to return quality codes. Defaults to true
#'@param long_format TRUE/FALSE to return data in long format, rather than wide (e.g. a spreadsheet). Long is useful for plotting with ggplot
#'
#'@return A tibble with the data in the file
#'
#'@examples 
#'\dontrun{
#'AQWPLoad("AQWP.json")
#'}
#'
#'@export

AQWPLoad<-function(filename,qual_codes=TRUE,long_format=TRUE) #return data in long format)
{
  f<-file(filename)
  json<-readLines(f,warn=FALSE)
  close(f)
  
  if(!jsonlite::validate(json)) #sometime AQQP fails and returns HTML
  {
    warning("file does not contain json. water.data.sa.gov.au may have failed, even if the request was valid. Check the download and try again. returned NULL")
    return(NULL)
  }
  X<-jsonlite::fromJSON(json)
  
  if(X$NumRows<1)
  {
    warning(paste("successful query but no data returned for the site, parameter and dates requested for",X$Datasets$Identifier))
    return(NULL)
  }
  
  colnames<-paste(X$Datasets$LocationIdentifier,X$Datasets$Parameter,X$Datasets$Unit,sep="_")
  Time<-X$Rows$Timestamp
  Time<-as.POSIXct(Time,format="%Y-%m-%dT%H:%M:%S",tz = "UTC") #time will be in local time without daylight savings. 
  Time<-tibble::as_tibble(Time) %>% stats::setNames("Time")
  nvar<-nrow(X$Datasets)
  
  dat<-lapply(X$Rows$Points,"[[","Value")
  mask<-sapply(dat,function(x) {ifelse(is.null(x),TRUE,FALSE)}) #patch NULL rows
  dat[mask]<-list(rep(NA,2))
  dat<-matrix(unlist(dat),nrow=X$NumRows,ncol=nrow(X$Datasets),byrow=TRUE)
  colnames(dat)<-colnames
  dat<-Time %>% dplyr::bind_cols(tibble::as_tibble(dat)) #use tibbles, data.frame can't handle spaces or / in column names
  
  if(qual_codes)
  {
    codes<-lapply(X$Rows$Points,"[[","GradeCode")
    mask<-sapply(codes,function(x) {ifelse(is.null(x),TRUE,FALSE)}) #patch NULL rows
    codes[mask]<-list(rep(NA,2))
    codes<-matrix(unlist(codes),nrow=X$NumRows,ncol=nrow(X$Datasets),byrow=TRUE)
    
    colnames(codes)<-paste(colnames,"Qual",sep="+")
    codes<-Time %>% dplyr::bind_cols(tibble::as_tibble(codes))
    dat<-dplyr::left_join(dat,codes,by="Time")
  }
  
  if(long_format)
  {
    
    dat<-dat %>% tidyr::gather("Site","Value",-Time) %>% 
      tidyr::separate("Site",c("Site","Parameter","Unit"),sep="_") 
    
    if(qual_codes)
    {
   
      dat<-dplyr::left_join(dat %>% dplyr::filter(!stringr::str_detect(.data$Unit,"Qual")),
                   dat %>% dplyr::filter(stringr::str_detect(.data$Unit,"Qual")) %>% 
                     dplyr::mutate(Unit=gsub("\\+Qual","",.data$Unit)),by=c("Time","Parameter","Site","Unit")) %>% 
        dplyr::rename("Value"=.data$Value.x,"Qual"=.data$Value.y)
    }
  }
  return(dat)
}


#' Function to download data from https://water.data.sa.gov.au 
#'
#'@description
#'For most inputs, valid options will be returned if an unexpected input is provided. 
#'The exception are \strong{Location} and \strong{Dataset}, if the location, or dataset for that location, don't exist no data will be returned.
#'Browse the Export tab on https://water.data.sa.gov.au to find \strong{Location} and \strong{Dataset} that exists.
#'
#'@param Location A string or vector of strings, with site numbers, e.g. "A4261001"
#'@param Dataset  A string or vector of strings, with dataset names, as expected by AWQP, e.g. "Tide Height.Best Available--Continuous"
#'@param Unit  A string or vector of strings, with units, e.g. "Metres" or "mg/L". If only 1 is string is provided it will be used for each site in Location
#'@param file Location and name of json file to download. Defaults to "AQWP.json".
#'@param Interval Interval of output, e.g. "PointsAsRecorded", or "Daily"
#'@param Step How many intervals e.g. 15 with Interval="Minutely" returns 15 minute data.
#'@param Calculation For larger intervals, what calculation to do, e.g. "Aggregate" (average) or "Maximum"
#'@param DateRange Period of data to return, e.g. "EntirePeriodOfRecord" or "Custom". "Years1" seems to not work on AWQP.
#'@param StartTime Start Date and Time if DateRange="Custom", in a format that as.POSIXct will convert, e.g 2000-01-01 00:00
#'@param EndTime End Date and Time if DateRange="Custom", in a format that as.POSIXct will convert, e.g 2001-01-02 00:00
#'@param Calendar When to start the periods, e.g. "WATERDAY9AM"
#'
#'@return The link created to download the data, which is useful for debugging. The data is saved to "file", that can then be read in with AQWPLoad()
#'
#'@examples
#'\dontrun{
#' Location=c("A4260633","A4261209","A4260572")
#' Dataset=rep("Tide Height.Best Available--Continuous",3)
#' Unit=rep("Metres",3)
#' S="2020-01-01 00:00"
#' E="2020-01-02 00:00"
#' AQWPDownload(Location,Dataset,Unit,DateRange="Custom",
#' StartTime=S,EndTime=E,file=tempfile("AQWP",fileext=".json"))
#' }
#'
#'@importFrom utils download.file
#'@export

AQWPDownload<-function(Location,Dataset,Unit,file="AQWP.json",
                       Interval="Daily",Calculation="Aggregate",Calendar="CALENDARYEAR",Step=1,
                       DateRange="EntirePeriodOfRecord",StartTime=NULL,EndTime=NULL)
{
  DateRanges<-c("EntirePeriodOfRecord","OverlappingPeriodOfRecord","Today","Days7","Days30","Months6","Years1","Custom")
  Calendars<-c("WATERDAY6AM","CALENDARYEAR","WATERDAY9AM")
  Intervals<-c("PointsAsRecorded","Minutely","Hourly","Daily","Monthly","Yearly")
  Calculations<-c("Instantaneous","Aggregate","Minimum","Maximum")
  
  
  #unit lookup table
  unitconversion<-tibble::as_tibble(c(tibble::as_tibble_col(column_name="Unit",c("Metres",
                                                                 "NTU",
                                                                 "EC",
                                                                 "Celcius",
                                                                 "kPa",
                                                                 "RFU",
                                                                 "mg/L",
                                                                 "pH",
                                                                 "percent",
                                                                 "W/m2",
                                                                 "Degrees",
                                                                 "m/s",
                                                                 "m3/s",
                                                                 "ML/d",
                                                                 "ML",
                                                                 "m3")),
                                      tibble::as_tibble_col(column_name="ID",c(82,
                                                               13,
                                                               45,
                                                               169,
                                                               145,
                                                               74,
                                                               35,
                                                               133,
                                                               152,
                                                               139,
                                                               52,
                                                               185,
                                                               239,
                                                               241,
                                                               190,
                                                               191
                              ))))
  
  if(!any(stringr::str_detect(DateRanges,DateRange)))
  {
    stop(paste("Bad Date range. Options are:",paste(DateRanges,collapse = ", ")))
  }
  
  if(!any(stringr::str_detect(Calendars,Calendar)))
  {
    stop(paste("Bad Calendar range. Options are:",paste(Calendars,collapse = ", ")))
  }
  
  if(!any(stringr::str_detect(Interval,Intervals)))
  {
    stop(paste("Bad Interval. Options are:",paste(Intervals,collapse = ", ")))
  }
  
  if(!any(stringr::str_detect(Calculation,Calculations)))
  {
    stop(paste("Bad Calculation. Options are:",paste(Calculations,collapse = ", ")))
  }
  
  if(Interval=="PointsAsRecorded" & Calculation!="Instantaneous")
  {
    stop("calculation must be Instantaneous for points as recorded data")
  }
  
  for(theunit in Unit)
  {
    if(!any(stringr::str_detect(theunit,unitconversion$Unit)))
    {
      stop(paste("Unit",theunit,"not supported. Options are:",paste(sort(unitconversion$Unit),collapse=", ")))
    }
  }
  
  if(length(Location)!=length(Dataset) | length(Location)!=length(Unit))
  {
    if(length(Dataset)==1 & length(Unit)==1)
    {
      Dataset=rep(Dataset,length(Location))
      Unit=rep(Unit,length(Location))
      
    }else
    {
      stop("Length of Location, Dataset and Unit do not match")
    }
  }
    
  #lookup Unit ID numbers
  unitIDs<-tibble::as_tibble(Unit) %>% dplyr::left_join(unitconversion,by=c("value"="Unit")) %>% dplyr::pull(.data$ID)
  
  
  if(DateRange=="Custom")
  {
    StartTime<-gsub("pct","%",format(as.POSIXct(StartTime),"%Y-%m-%d %Hpct3A%M"))
    EndTime<-gsub("pct","%",format(as.POSIXct(EndTime),"%Y-%m-%d %Hpct3A%M"))
    DateRange=paste0(DateRange,"&StartTime=",StartTime,"&EndTime=",EndTime)
  }
  
  link<-paste0("https://water.data.sa.gov.au/Export/BulkExportJson?DateRange=",DateRange,
               "&TimeZone=9.5&Calendar=",Calendar,
               "&Interval=",Interval,
               "&Step=",Step,
               "&ExportFormat=json&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=False&IncludeQualifiers=False&IncludeInterpolationTypes=False")
  
  for(i in 1:(length(Location)))
  {
    link<-paste0(link,"&Datasets[",i-1,"].DatasetName=",Dataset[i],"%40",Location[i],
                 "&Datasets[",i-1,"].Calculation=",Calculation,
                 "&Datasets[",i-1,"].UnitId=",unitIDs[i])
  }
  
  link<-gsub(" ","%20",link)
  
  #increase download timeout, website can be s l o w for long or high frequency dataset
  X<-httr::GET(link)
  bin <- httr::content(X, "raw")
  writeBin(bin, file)
  return(link)
  
}