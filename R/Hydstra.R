#' Function to export Hydstra data, similar to HYCSV
#'
#'@param sites A station number or vector of station numbers, as a string (e.g. "A4261001")
#'@param VarFrom A variable that exists for the station to convert from, for example water level, as a string e.g. "100.00".
#'@param VarTo A variable can be output for the station, for example flow, as a string e.g. "141.00".
#'@param startdate First period of data, including the time, e.g. "09:00_01/01/2017".
#'@param enddate Last period of data, including the time, e.g. "09:00_30/07/2017".
#'@param Interval Period of data required, e.g. HOUR, DAY, MONTH
#'@param path Where to save the output time series, defaults to getwd()
#'
#'@return A file is created with the data requested. Nothing is returned to the R environment
#'
#'@examples HydstraExport("A2390519","100.00","141.00")
#'
HydstraExport<-function(sites,varFrom,varTo,startdate="09:00_01/01/1980",enddate="09:00_01/01/1980",Interval="DAY",path=NULL,file="Auto.ini")
{

  cat("[Version]\n",file=file)
  cat("Version=5\n",file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)
  cat("[Auto_Data List]\n",file=file,append=TRUE)
  cat("HR1 = PRN R Script Auto Extract\n",file=file,append=TRUE)
  cat("\n",file=file,append=TRUE)
  #  cat("[Auto_Data PreCommands]",file=file,append=TRUE)
  #  Command = del %BaseDir%\*.csv
  #  Command = del %BaseDir%\*.xslx
  #
  cat("[Auto_Data Config]\n",file=file,append=TRUE)

  cat(paste0("PeriodVar= ",varFrom,"\n"),file=file,append=TRUE)#     ; to calculate period of record for these reports
  cat("\n",file=file,append=TRUE)
  cat("StartMonth = 1\n",file=file,append=TRUE) #dont know what these next 3 rows do...
  cat("FlowHours = 2400\n",file=file,append=TRUE)
  cat("SiteFolders = no\n",file=file,append=TRUE)
  cat(paste0("BaseDir = ",gsub("/","\\\\",getwd()),"\n"),file=file,append=TRUE) #may not work, wrong slash, orig: #BaseDir = &HYD-usr_TaskAutomation.Rodwell_Ck_Reporting
  cat(paste0("LogName = %basedir%\\Hybatch.log\n"),file=file,append=TRUE) #LogName = &HYD-usr_TaskAutomation.Rodwell_Ck_Reporting\Logfile=files\HyBatch.log
  cat("\n",file=file,append=TRUE)
  cat("[Auto_Data PostCommands]\n",file=file,append=TRUE)
  cat("command = del %BaseDir%\\Headers.ini\n",file=file,append=TRUE)
  cat("[HR1]\n",file=file,append=TRUE)
  cat(paste0("PeriodVar= ",varFrom,"\n"),file=file,append=TRUE)#     ; to calculate period of record for these reports
  cat("\n",file=file,append=TRUE)
  for(s in sites) {
    cat(paste0("Params = DATA ",s," AT ",varFrom," ",varTo," MEAN\n"),file=file,append=TRUE)  #TODO - allow for other data types than mean, and not just AT
  }
  Multiplier<-"1.00" #TODO, allow this as an input, how many days as part of the interval
  NegativeTimeOffset<-"0.00"
  Outfilename<-paste0(paste0(sites,collapse=""),"_",varTo,"_",Interval,".csv")
  cat(paste0("Params = TIME ",Interval," ",Multiplier," ",NegativeTimeOffset," ",startdate," 09:00_n/n/n start %basedir%\\",Outfilename," No Yes No \"HH:II:EE DD/MM/YYYY\"\n"),file=file,append=TRUE)
  cat("Program = HYCSV",file=file,append=TRUE)
  file.copy(file,paste0("R:\\WaterMon\\TS_Manage\\Task_Automation\\RScript\\"),overwrite=TRUE)
  system("R:\\WaterMon\\TS_Manage\\Task_Automation\\RScript\\RunAutoini.bat")

  if(!is.null(path))
  {
    if(!dir.exists(path)){
      print(paste("Path",path,"Doesn't exist. File saved to",getwd()))
    }else {
    file.copy(Outfilename,path,overwrite=TRUE)
    file.remove(Outfilename)
    }
  }
}



#' Function to load in an Aquarius json file, downloaded from water.data.sa.gov.au
#'
#'@param filename A file downloaded from the Export Data tab on water.data.sa.gov.au, or using AQWPDownload()
#'@param qual_codes TRUE/FALSE to return quality codes. Defaults to true
#'@param long_format TRUE/FALSE to return data in long format, rather than wide (e.g. a spreadsheet). Long is useful for plotting with ggplot
#'
#'@return A tibble with the data in the file
#'
#'@examples AQWPLoad("downloaded.json")
#'
AQWPLoad<-function(filename,qual_codes=TRUE,long_format=TRUE) #return data in long format)
{
  f<-file(filename)
  json<-readLines(f,warn=FALSE)
  close(f)
  X<-fromJSON(json)
  
  colnames<-paste(X$Datasets$LocationIdentifier,X$Datasets$Parameter,X$Datasets$Unit,sep="_")
  Time<-X$Rows$Timestamp
  Time<-as.POSIXct(Time,format="%Y-%m-%dT%H:%M:%S",tz = "UTC") #time will be in local time without daylight savings. 
  Time<-as_tibble(Time) %>% setNames("Time")
  nvar<-nrow(X$Datasets)
  
  dat<-lapply(X$Rows$Points,"[[","Value")
  mask<-sapply(dat,function(x) {ifelse(is.null(x),TRUE,FALSE)}) #patch NULL rows
  dat[mask]<-list(rep(NA,2))
  dat<-matrix(unlist(dat),nrow=X$NumRows,ncol=nrow(X$Datasets),byrow=TRUE)
  colnames(dat)<-colnames
  dat<-Time %>% bind_cols(as_tibble(dat)) #use tibbles, data.frame can't handle spaces or / in column names
  
  if(qual_codes)
  {
    codes<-lapply(X$Rows$Points,"[[","GradeCode")
    mask<-sapply(codes,function(x) {ifelse(is.null(x),TRUE,FALSE)}) #patch NULL rows
    codes[mask]<-list(rep(NA,2))
    codes<-matrix(unlist(codes),nrow=X$NumRows,ncol=nrow(X$Datasets),byrow=TRUE)
    
    colnames(codes)<-paste(colnames,"Qual",sep="+")
    codes<-Time %>% bind_cols(as_tibble(codes))
    dat<-left_join(dat,codes,by="Time")
  }
  
  if(long_format)
  {
    
    dat<-dat %>% gather("Site","Value",-Time) %>% 
      separate("Site",c("Site","Parameter","Unit"),sep="_") 
    
    if(qual_codes)
    {
   
      dat<-left_join(dat %>% filter(!str_detect(Unit,"Qual")),
                   dat %>% filter(str_detect(Unit,"Qual")) %>% mutate(Unit=gsub("\\+Qual","",Unit)),by=c("Time","Parameter","Site","Unit")) %>% 
        rename("Value"=Value.x,"Qual"=Value.y)
    }
  }
  return(dat)
}




Location=c("A4260572")
Dataset="Tide Height.Best Available--Continuous"
Unit="Metres"

#' Function to download data from water.data.sa.gov.au
#'
#'@param Location A string or vector of strings, with site numbers, e.g. "A4261001"
#'@param Dataset  A string or vector of strings, with dataset names, as expected by AWQP, e.g. "Tide Height.Best Available--Continuous"
#'@param Unit  A string or vector of strings, with units, e.g. "Metres"
#'@param file Location and name of json file to download
#'@param Interval Interval of output, e.g. "PointsAsRecorded, or "Daily"
#'@param Step How many intervals e.g. 15 with Interval="Minutely" returns 15 minute data.
#'@param Calculation For larger intervals, what calculation to do, e.g. "Aggregate" (average) or "Maximum
#'@param Calendar When to start the periods, e.g. "WATERDAY_9AM"
#'@param DateRange Period of data to return, e.g. "EntirePeriodOfRecord" or "Custom"
#'@param StartTime Start Date and Time if DateRange="Custom", in a format that as.POSIXct will convert, e.g %Y-%m-%d %H:%M
#'@param EndTime End Date and Time if DateRange="Custom", in a format that as.POSIXct will convert, e.g %Y-%m-%d %H:%M
#'
#'Note for big datasets, increase download timeout option first using options(timeout=1000000). The default is 60 (seconds)
#'Valid options will be output if an unexpected input is provided
#'
#'@return nothing to the environment. Saves a file to "file", that can then be read in with AQWPLoad()
#'
#'@examples
#'Location=c("A4260633","A4261209","A4260572")
#'Dataset=rep("Tide Height.Best Available--Continuous",3)
#'Unit=rep("Metres",3)
#'AQWPDownload(Location,Dataset,Unit,DateRange="Years1")

AQWPDownload<-function(Location,Dataset,Unit,file="AQWP.json",
                       Interval="Daily",Calculation="Aggregate",Calendar="WATERDAY_9AM",Step=1,
                       DateRange="EntirePeriodOfRecord",StartTime=NULL,EndTime=NULL)
{
  DateRanges<-c("EntirePeriodOfRecord","OverlappingPeriodOfRecord","Today","Days7","Days30","Months6","Years1","Custom")
  Calendars<-c("WATERDAY_6AM","CALENDARYEAR","WATERDAY_9AM")
  Intervals<-c("PointsAsRecorded","Minutely","Hourly","Daily","Monthly","Yearly")
  Calculations<-c("Instantaneous","Aggregate","Minimum","Maximum")
  
  
  #unit lookup table
  unitconversion<-as_tibble(c(as_tibble_col(column_name="Unit",c("Metres",
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
                              as_tibble_col(column_name="ID",c(82,
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
  
  if(!any(str_detect(DateRanges,DateRange)))
  {
    print(paste("Bad Date range. Options are:",paste(DateRanges,collapse = ", ")))
    return(-1)
  }
  
  if(!any(str_detect(Calendars,Calendar)))
  {
    print(paste("Bad Calendar range. Options are:",paste(Calendars,collapse = ", ")))
    return(-1)
  }
  
  if(!any(str_detect(Interval,Intervals)))
  {
    print(paste("Bad Interval. Options are:",paste(Intervals,collapse = ", ")))
    return(-1)
  }
  
  if(!any(str_detect(Calculation,Calculations)))
  {
    print(paste("Bad Calculation. Options are:",paste(Calculations,collapse = ", ")))
    return(-1)
  }
  
  if(Interval=="PointsAsRecorded" & Calculation!="Instantaneous")
  {
    print("calculation must be Instantaneous for points as recorded data")
    return(-1)
  }
  
  for(theunit in Unit)
  {
    if(!any(str_detect(theunit,unitconversion$Unit)))
    {
      print(paste("Unit",theunit,"not supported. Options are:",paste(sort(unitconversion$Unit),collapse=", ")))
      return(-1)
    }
  }
  
  if(length(Location)!=length(Dataset) | length(Location)!=length(Unit))
  {
    print("Length of Location, Dataset and Unit do not match")
    return(-1)
  }
    
  #lookup Unit ID numbers
  unitIDs<-as_tibble(Unit) %>% left_join(unitconversion,by=c("value"="Unit")) %>% pull(ID)
  
  
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
               "&ExportFormat=json&TimeAligned=True&RoundData=False&IncludeGradeCodes=True&IncludeApprovalLevels=False&IncludeInterpolationTypes=False")
  
  for(i in 1:(length(Location)))
  {
    link<-paste0(link,"&Datasets[",i-1,"].DatasetName=",Dataset[i],"%40",Location[i],
                 "&Datasets[",i-1,"].Calculation=",Calculation,
                 "&Datasets[",i-1,"].UnitId=",unitIDs[i])
  }
  
  link<-gsub(" ","%20",link)
  
  #increase download timeout, website can be s l o w for long or high frequency dataset
  options(timeout=getOption('timeout')*100)
  download.file(link,file)
}