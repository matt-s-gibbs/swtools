
#which period is the homogenous (good) data, after=TRUE means the period after "year"

#' Correct a slope change in a rainfall data set based on another site 
#'
#' If the break point of a non-homogenous rainfall station has been identified (potentially using \link[SWTools]{SILOCheckConsistency}), correct the data on one side of the breakpoint
#' 
#' The method of cumulative residuals outlined in [Annex 4](https://www.fao.org/4/x0490e/x0490e0l.htm#annex%204.%20statistical%20analysis%20of%20weather%20data%20sets%201) of Allen et al. (1998) has been used.
#' That is, two linear regressions between the annual rainfall totals are calculated \code{P_s_correct~P_s_reference} over the periods year_start:year_break and year_break:year_end
#' For the period to correct (after the breakpoint if \code{after=TRUE}) an annual scaling factor is calculated from the ratio of the predicted rainfall total from the two regression equations, based on the rainfall total for each year at the reference site.
#' This scaling factor is then applied to the daily rainfall data for that year.
#' 
#' @seealso 
#' \link[SWTools]{SILOLoad}, \link[SWTools]{SILOSiteSummary}, \link[SWTools]{SILOQualityCodes},\link[SWTools]{SILOCorrectSite}
#'
#' @references 
#' 
#' Chang, M., and Lee, R. (1974) Objective double-mass analysis, Water Resour. Res., 10( 6), 1123– 1126, doi:10.1029/WR010i006p01123.
#' 
#' @param X A list of SILO station data, in the format created by \code{\link[SWTools]{SILOLoad}}
#' @param s_correct Station number that exists in \code{X} to correct
#' @param s_reference Station number that exists in \code{X} to used for the correction
#' @param year_break year in the time series that the break points exists
#' @param year_start first year of data (before \code{year_break}) to to develop the first linear regression between \code{s_correct} and \code{s_reference}. Defaults to the start of the dataset
#' @param year_end last year of data (after \code{year_break}) to to develop the second linear regression between \code{s_correct} and \code{s_reference}. Defaults to the end of the dataset
#' @param after TRUE/FALSE value, indicating if the homogeneous data to develop the relationship to correct the non-homogeneous data is after the breakpoint (\code{TRUE}) or before (\code{FALSE}).
#' @param plot if specified, the file (including path if necessary) to save a scatter plot of the annual rainfall totals, including regression equations used to correct the non-homogeneous data.
#' 
#' @return A list with the same structure as \code{X}, with the element for \code{s_correct} updated with the corrections on one side of the breakpoint year.
#' @export
#'
#' @examples
#' \dontrun{
#' stations<-c("23313","23302","23300","23317","23725","23705")
#' SILODownload(stations)
#' X<-SILOLoad(stations,startdate="1891-01-01",enddate="2020-12-31")
#' X<-SILOCorrectSite(X,"23313","23705",1970,after=FALSE)
#' }
#' 
SILOCorrectSite<-function(X,s_correct,s_reference,year_break,year_start=NULL,year_end=NULL,after=TRUE,plot=NA)
{
if(is.null(X[[s_correct]])) stop(paste("SILO list does not contain",s_correct))
if(is.null(X[[s_reference]])) stop(paste("SILO list does not contain",s_reference))

rain_correct<-hydroTSM::daily2annual(X[[s_correct]]$tsd$Rain,FUN=sum)
rain_reference<-hydroTSM::daily2annual(X[[s_reference]]$tsd$Rain,FUN=sum)

col<-zoo::index(hydroTSM::daily2annual(X[[s_correct]]$tsd$Rain,FUN=sum))
col=ifelse(col<as.Date(paste0(year_break,"-01-01")),"red","blue")

if(is.null(year_end)) year_end=max(lubridate::year(rain_correct))
if(is.null(year_start)) year_start=min(lubridate::year(rain_correct))

if(year_start>year_break | year_start>year_end) stop("start year is not the earliest year")
if(year_end<year_break) stop("end year must be later than break year")

data=data.frame(x=stats::window(rain_reference,start=as.Date(paste0(year_start,"-01-01")),end=as.Date(paste0(year_break-1,"-12-31"))),
                y=stats::window(rain_correct,start=as.Date(paste0(year_start,"-01-01")),end=as.Date(paste0(year_break-1,"-12-31"))))
lm_first<-lm(y~x,data=data)

data<-data.frame(x=stats::window(rain_reference,start=as.Date(paste0(year_break,"-01-01")),end=as.Date(paste0(year_end,"-12-31"))),
                 y=stats::window(rain_correct,start=as.Date(paste0(year_break,"-01-01")),end=as.Date(paste0(year_end,"-12-31"))))
lm_second<-lm(y~x,data=data)

if(!is.na(plot))
{
  grDevices::tiff(paste0(plot,".tiff"),width=9,height=9,units="cm",res=1000,compression="lzw")
  graphics::par( mai = c(0.8, 0.8, 0.1, 0.1))
  
  plot(rain_reference,rain_correct,
       bg=col,col=col,
       xlab=paste("Station",s_reference,"(mm/year)"),
       ylab=paste("Station",s_correct,"(mm/year)"),
       cex.lab=0.8, cex.axis=0.8,
       pch=21,cex=0.5)
  
  graphics::abline(lm_first,col="red")
  graphics::abline(lm_second,col="blue")
  graphics::legend(min(rain_reference)*1.01, max(rain_correct)*0.99, 
                   legend=c(paste0(year_start,"-",year_break-1),paste0(year_break,"-",year_end)),
        col=c("red", "blue"), lty=1,cex=0.5)
  grDevices::graphics.off()
}

#calculate an annual scaling factor
corrections<-tibble::tibble(year=lubridate::year(zoo::index(rain_correct)),orig=as.numeric(rain_correct),corrected=as.numeric(rain_correct),reference=as.numeric(rain_reference))

data=data.frame(x=rain_reference)
x=stats::predict(lm_first,data)-stats::predict(lm_second,data)

corrections<-corrections %>%  dplyr::mutate(correction=x) %>% 
  dplyr::rowwise() %>%
  dplyr::mutate(corrected=ifelse(after,ifelse(.data$year<year_break,.data$corrected-.data$correction,.data$corrected),
                                                           ifelse(.data$year>year_break,.data$corrected+.data$correction,.data$corrected))) %>% 
  dplyr::ungroup() %>% 
  dplyr::mutate(factor=.data$corrected/.data$orig)

#apply to the daily data
daily<-X[[s_correct]]$tsd$Rain
daily<-tibble::tibble(Date=zoo::index(daily),Rain=as.numeric(daily))

daily<-daily %>% dplyr::mutate(year=lubridate::year(.data$Date)) %>% 
  dplyr::left_join(corrections %>%dplyr::select(.data$year,.data$factor),by="year") %>% 
  dplyr::mutate(Rain=.data$Rain*factor)

X[[s_correct]]$tsd$Rain<-daily %>% dplyr::pull(.data$Rain)

return(X)
}
