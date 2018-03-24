#' Write a csv file that can be loaded into a model, e.g. Source
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param col Name of a column in a silo file to write out
#' @param filename file to write to.
#'
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILOWriteforSource(X,"Rain","Rainfall.csv")

SILOWriteforSource<-function(SILO,col,filename)
{
  dat<-lapply(SILO,function(x) x$tsd[,col])
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
  colnames(dat)<-names(SILO)
  dat<-zoo::zoo(dat,zoo::index(SILO[[1]]$tsd))
  write.csv(dat,filename,row.names = format(zoo::index(dat),"%d/%m/%Y"),quote = FALSE)
}