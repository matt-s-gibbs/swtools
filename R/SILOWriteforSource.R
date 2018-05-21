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