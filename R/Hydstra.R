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
