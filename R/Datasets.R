#' DEW colour scheme
#'
#' Colours derived from the technical report template, Weekly email, and similar colours using color.io website
#' 
#' @docType data
#' 
#' @usage data(DEWcol)
#' 
#' @format A character vector of colours in hex format
#' 
#' @keywords datasets
#' 
#' @examples 
#' data(DEWcol)
#' dat<-data.frame(x=runif(20),y=runif(20),z=rep(c("a","b"),10))
#' ggplot(dat)+geom_point(aes(x,y,colour=z))+scale_colour_manual(values=DEWcol)