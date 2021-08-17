#' Write a SILO time series to a csv file in the format expected by eWater Source
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param col Name of a column in a silo file to write out, e.g. Rain
#' @param filename file to write to.
#' @param scalefactor factor to scale the data by. Defaults to 1. Useful for Pan evap or rainfall scaling. Could also be a vector, with a value for each station in SILO
#'
#' @return Nothing to the R environment. SILO data is written to "filename".
#'
#' @examples  
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' SILOWriteforSource(X,"Rain",tempfile("Rainfall",fileext=".csv"))
#' }
#' 
#' @export
#' @importFrom utils write.csv

SILOWriteforSource<-function(SILO,col,filename,scalefactor=1)
{
  dat<-lapply(SILO,function(x) x$tsd[,col])
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
  colnames(dat)<-names(SILO)
  dat<-zoo::zoo(dat,zoo::index(SILO[[1]]$tsd))
  
  #if scalefactor is a vector of factors, do matrix multiplication
  if(length(scalefactor)>1){
    if(ncol(dat)!=length(scalefactor)) warning(paste("number of scaling factors,",length(scalefactor),",does not match number of columns,",ncol(dat)))
    write.csv(dat %*% scalefactor,filename,row.names = format(zoo::index(dat),"%d/%m/%Y"),quote = FALSE)  
  }else  {
    write.csv(dat * scalefactor,filename,row.names = format(zoo::index(dat),"%d/%m/%Y"),quote = FALSE)
  }
}

#' Function to inport a Source .res.csv File
#' Returns data (as a Data Frame,  Zoo, or tibble) as a time Series with all Results
#' Read Source .res.csv file into a data table or zoo time series
#'
#' @param resFile A character string representing the full file path of the .res.csv file
#' @param returnType A character string to set the return type: "z", "t", "df". If not matching "t" (tibble) or "z" (zoo), data frame returned.
#'
#' @return Data in the format selected with all data read in from the Source .res.csv file
#'
#' @examples  
#' \dontrun{
#' X = read_res.csv("./SWTools/extdata/Scenario1.res.csv",returnType="t")
#' }
#'
#' @export
#' @importFrom utils read.csv

read_res.csv <- function(resFile,returnType="df")
{
  s <-readLines(resFile,skipNul = TRUE)
  
  EOCline <- which(s=="EOC",arr.ind = TRUE)
  
  EOHline <- which(s=="EOH",arr.ind = TRUE)
  
  hline <- EOCline + 2
  
  numOutputs <- as.integer(s[EOCline+1])
  
  d <- read.csv(resFile,header  = FALSE,skip = EOHline,sep = ",",as.is=TRUE)
  
  allColHeaders <- read.csv(resFile,header = FALSE,skip = hline-1,nrows = numOutputs,as.is=TRUE)
  
  t<-ifelse(stringr::str_detect(allColHeaders$V11,allColHeaders$V8),allColHeaders$V11,paste0(allColHeaders$V8,".",allColHeaders$V11))
  
  colHeaders <- paste0(allColHeaders$V7,".",t)
  
  colnames(d) <- c("Date",colHeaders)
  d$Date<-as.Date(d$Date)
  
  d<-d[,!duplicated(colnames(d))] #Source seems to be outputting some functions twice and it causes problems using the data. Remove any duplicate columns
  
  if(returnType=="t")  return(tibble::as_tibble(d))
  if(returnType=="z")   return(zoo::zoo(d,order.by = d$Date))
  return(d)
  
}

#' Write an input set line for a piecewise lookup table from a csv file
#' 
#' @param folder Folder for where are the csv files with the lookup tables
#' @param csvfiles vector of files to turn into an input set line. 
#' File name should be the name of the pw table in Source, including the folder name if necessary, separated by "." (see example). 
#' The first row in the file should be column names, the same as used in Source, i.e. XValue and YValue
#' @param outputfile text file to save the lines to
#' 
#' @return Nothing to the R environment. Input set lines are written to "outputfile".
#' 
#' @examples 
#' \dontrun{
#' folder<-"C:/Source/tables"
#' csvfiles<-c("LowerLakesOps.pw_LakeTarget.csv","Operations.pw_NA_Lock5_16p8.csv")
#' outputfile<-"inputset.txt"
#' WritepwtoIS(folder,csvfiles,outputfile)
#' }
#' 
#' @export
#' @importFrom utils read.csv

WritepwtoIS<-function(folder,csvfiles,outputfile)
{
  con<-file(paste0(folder,"/",outputfile),"w")
  for(f in csvfiles)
  {
    x<-read.csv(paste0(folder,"/",f))
    name<-gsub(".csv","",f)
    sep<-c(rep("][",nrow(x)-1),"]]")
    writeLines(paste0("Functions.Variables.$",name,".Relationship=[[",paste0(x$XValue," ",x$YValue,sep,collapse="")),con)
  
  }
  close(con)
}