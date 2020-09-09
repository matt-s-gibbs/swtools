#' Write a csv file that can be loaded into a model, e.g. Source
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param col Name of a column in a silo file to write out
#' @param filename file to write to.
#' @param scalefactor factor to scale the data by. Defaults to 1. Useful for Pan evap or rainfall scaling. Could also be a vector, with a value for each station in SILO
#'
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILOWriteforSource(X,"Rain","Rainfall.csv")

SILOWriteforSource<-function(SILO,col,filename,scalefactor=1)
{
  dat<-lapply(SILO,function(x) x$tsd[,col])
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
  colnames(dat)<-names(SILO)
  dat<-zoo::zoo(dat,zoo::index(SILO[[1]]$tsd))
  
  #if scalefactor is a vector of factors, do matrix multiplication
  if(length(scalefactor)>1){
    if(ncol(dat)!=length(scalefactor)) print(paste("number of scaling factors,",length(scalefactor),",does not match number of columns,",ncol(dat)))
    write.csv(dat %*% scalefactor,filename,row.names = format(zoo::index(dat),"%d/%m/%Y"),quote = FALSE)  
  }else  {
    write.csv(dat * scalefactor,filename,row.names = format(zoo::index(dat),"%d/%m/%Y"),quote = FALSE)
  }
}

# Function to Read a Source .res.csv File

# Returns data (as a Data Frame,  Zoo, or tibble) as a time Series with all Results

#' Read Source .res.csv file into a data table or zoo time series
#'
#' @param resFile A character string representing the full file path of the .res.csv file
#'
#' @param returnType A character string to set the return type: "z", "t", "df". If not specified or not matching "t" (tibble) or "z" (zoo), data frame returned.
#'
#' @return Data in the format selected with all data read in from the Source .res.csv file
#'
#' @examples X = read_res.csv(file.choose(),returnType="df")
#'
#'

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
  
  if(returnType=="t")  return(tibble::as_tibble(d))
  if(returnType=="z")   return(zoo::zoo(d,order.by = d$Date))
  return(d)
  
}
