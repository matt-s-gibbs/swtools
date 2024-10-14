#' Check for homogeneity between SILO rainfall station data
#'
#' Compute tests on rainfall double mass curves and cumulative deviation in annual rainfall totals to test for consistency between a rainfall station and the average of another group of stations.
#' Non-homogeneity can occur for a number of reasons, such as interception from vegetation or buildings over time, moving of a station location, or due to interpolation of missing data or station closure
#'
#' Two tests are calculated by \code{SILOCheckConsistency}, which are outlined in [Annex 4](https://www.fao.org/4/x0490e/x0490e0l.htm#annex%204.%20statistical%20analysis%20of%20weather%20data%20sets%201) of Allen et al. (1998).
#' 
#' The first considers the residual errors in annual rainfall at a station, compared to the straight line (intercept=0) regression with the average annual rainfall from the other sites in \code{X}. 
#' The residuals should follow a normal distribution with mean zero and standard deviation s_y,x. The annual rainfall data is plotted to visually assess the homoscedasticity requirement (constant variance).
#' Ellipses for 80\% and 95\% confidence in rejecting the homogeneity hypothesis are plotted on the cumulative residuals figure. 
#' 
#' The second test tests for a break point in the plot of cumulative annual rainfall, commonly referred to as a double-mass analysis. 
#' This analysis is outlined in Allen et al. (1998) and also Chang and Lee (1974). A bootstrapped estimate of any breakpoint in the double-mass plot, indicating a change in the relationship between rainfall at the station and the average of all other stations,
#' is assessed using the method of Muggeo (2003), as provided in \link[segmented]{segmented}.
#' 
#' @seealso 
#' \link[SWTools]{SILOLoad}, \link[SWTools]{SILOSiteSummary}, \link[SWTools]{SILOQualityCodes},\link[SWTools]{SILOCorrectSite}
#'
#'
#' @references 
#' 
#' Chang, M., and Lee, R. (1974) Objective double-mass analysis, Water Resour. Res., 10( 6), 1123-1126, doi:10.1029/WR010i006p01123.
#' 
#' Allan, R., Pereira, L. and Smith, M. (1998) Crop evapotranspiration-Guidelines for computing crop water requirements-FAO Irrigation and drainage paper 56. 
#' 
#' Muggeo, V.M.R. (2003) Estimating regression models with unknown break-points. Statistics in Medicine 22, 3055-3071.
#'
#' @param X A list of SILO station data, in the format created by \code{\link[SWTools]{SILOLoad}}
#' @param folder Path to folder to save resulting images to. Will be created if it doesn't exist
#' @param pvallim p value limit of the break point detection to display the double mass break point. Defaults to p=0.05
#' @param changelim significant slope limit display the double mass break point. Defaults to a slope change of 0.025
#'
#' @return If folder is not specified (or \code{NA}) the plots are shown in the R environment. If folder is specified, a figure for each station in \code{X} is saved to \code{folder}. There are 4 panels on the figure:
#' \itemize{
#' \item Annual rainfall for a given station, against the average across all stations in X (except the station presented).
#' \item Cumulative residuals of the annual rainfall from the straight line regression shown in the first panel. Assuming the residuals are are independent random variables, this figure include ellipses representing 80th and 95th percentile that the hypothesis that there is no change in slope can be rejected.
#' \item double mass curve, plotting the cumulative annual rainfall for the station against the station average. If a breakpoint is identified, this is displayed on the plot.The colours represent the median quality code for each year, with the same colour palette as \code{\link[SWTools]{SILOQualityCodes}}
#' \item Residuals of the cumulative rainfall from the straight line fitted to the double mass curve.
#' } 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' SILOCheckConsistency(X,tempdir())
#' }

SILOCheckConsistency<-function(X,folder=NA,pvallim=0.05,changelim=0.025)
{

#point colours
lookup <- data.frame(Code = c(0, 23, 13, 15, 35, 25, 75, 
                              26), Quality = c("Station data, as supplied by Bureau", 
                                               "Nearby station, data from BoM", "Deaccumulated using nearby station", 
                                               "Deaccumulated using interpolated data", "interpolated from daily observations\nusing anomaly interpolation method", 
                                               "interpolated daily observations", "interpolated long term average", 
                                               "synthetic pan evaporation"))

#colours to shade codes, green to red, derived from
#rev(RColorBrewer::brewer.pal(7,"RdYlGn"))
colcode<-c("#1A9850", "#91CF60", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027")
colcode<-tibble::tibble(cols=c(colcode, colcode[1]),code=lookup$Code)

#extract the rainfall data
rain<-zoo::zoo(sapply(X,function(x) x$tsd$Rain),zoo::index(X[[1]]$tsd))
qual<-zoo::zoo(sapply(X,function(x) x$tsd$Srn),zoo::index(X[[1]]$tsd))

if(!is.na(folder))
{
  if(!dir.exists(folder)) dir.create(folder)
}

for(station in 1:ncol(rain))
{
  
  stationname<-names(rain)[station]
  
  if(!is.na(folder)) grDevices::tiff(paste0(folder,"/",stationname,"-QA.tiff"),width=19,height=16,units="cm",res=1000,compression="lzw")
  graphics::par(mfrow=c(2,2), mai = c(0.8, 0.8, 0.3, 0.1))

  s1<-hydroTSM::daily2annual(rain[,station],FUN=sum)
  pointcol<-hydroTSM::daily2annual(qual[,station],FUN=stats::median)
  pointcol<-tibble::tibble(code=as.numeric(pointcol)) %>% dplyr::left_join(colcode,by=c("code"))
  
  if(ncol(rain)>2)
  {
    s2<-zoo::zoo(rowMeans(rain[,-station]),zoo::index(X[[1]]$tsd))
  }else
  {
    s2<-rain[,-station]
  }
  
  s2<-hydroTSM::daily2annual(s2,FUN=sum)
  
  #annual rainfall
  dati=data.frame(x=as.numeric(s2),y=as.numeric(s1))
  out.lm<-lm(y~x+0,data=dati)
  
  range=c(min(s1,s2),max(s1,s2))
  plot(s2,s1,xlab="Average of other stations (mm)",ylab=paste("Station",stationname,"(mm)"),
       xlim=range,ylim=range)
  graphics::title(main="Annual rainfall",cex.main=1)
  graphics::abline(out.lm)
  graphics::grid()
  label<-paste0("Slope=",round(out.lm$coefficients[1],3))
  graphics::text(mean(s2)*1.25,mean(s1)*0.9,label)
  
  #elipse calcs
  # Create Table of z Values for ellipsis
  Zp <- data.frame(p=c(50,60,70,75,80,85,90,95),zp=c(0.00,0.25,0.52,0.67,0.84,1.04,1.28,1.64))
  
  eps1 <- out.lm$residuals # the residuals of Y ~ X
  n<-length(eps1)
  p <- 80 # p is the level of confidence required
  pZ <- Zp[match(p,Zp[,1]),2] # Z value at p
  alpha <- n/2
  beta <- (n/(n-1)^0.5)*pZ*stats::sd(eps1)
  elipse80 <- sqrt(abs((beta^2)*(1-((1:n-alpha)/alpha)^2)))
  
  p <- 95 # p is the level of confidence required
  pZ <- Zp[match(p,Zp[,1]),2] # Z value at p
  alpha <- n/2
  beta <- (n/(n-1)^0.5)*pZ*stats::sd(eps1)
  elipse95 <- sqrt(abs((beta^2)*(1-((1:n-alpha)/alpha)^2)))
  
  ymin<-min(-elipse95,cumsum(eps1))
  ymax<-max(elipse95,cumsum(eps1))
  
  plot(zoo::index(s2),cumsum(eps1),xlab="year",ylab="Cumulative rainfall residuals (mm)",
       ylim=c(ymin,ymax))
  graphics::title(main="Residuals of annual rainfall to straight line",cex.main=1)
  graphics::lines(zoo::index(s2),elipse80,col="grey")
  graphics::lines(zoo::index(s2),-elipse80,col="grey")
  graphics::lines(zoo::index(s2),elipse95)
  graphics::lines(zoo::index(s2),-elipse95)
  graphics::text(zoo::index(s2)[floor(length(s2)/2)],max(elipse80)*0.9,"80%",col="grey")
  graphics::text(zoo::index(s2)[floor(length(s2)/2)],max(elipse95)*0.95,"95%")
 
  #now double mass
  
  s1=cumsum(s1)
  s2=cumsum(s2)
  
  dati=data.frame(x=as.numeric(s2),y=as.numeric(s1))
  out.lm<-lm(y~x+0,data=dati)
  
  #the simplest example: the starting model includes just 1 covariate 
  #.. and 1 breakpoint has to be estimated for that
  o.seg<-segmented::segmented(out.lm) #1 breakpoint for x
  
  pval<-segmented::pscore.test(o.seg)$p.value
  
  plot(dati$x,dati$y,
       col=pointcol$cols,
       pch=19,
       xlab="Average of other stations (mm)",ylab=paste("Station",stationname,"(mm)"))
  graphics::title(main="Double mass curve",cex.main=1)
  graphics::grid()
  #if significant
  if(pval<pvallim & abs(o.seg$coefficients[2])>changelim & (length(o.seg$coefficients)>1))
  {
    segmented::plot.segmented(o.seg,add=TRUE,link=FALSE,lwd=2,col=c("#525252","#525252"))
    segmented::lines.segmented(o.seg,col="black",pch=19,bottom=FALSE,lwd=2) #for the CI for the breakpoint
    segmented::points.segmented(o.seg,col="black",pch=19, link=FALSE)
    
    pvaltext<-ifelse(pval<0.001,formatC(pval, format = "e", digits = 2),round(pval,3))
    
    years<-array(0,3)
    psi<-segmented::confint.segmented(o.seg)
    for(i in 1:3) years[i]<-substr(zoo::index(s2[(max(which(s2<psi[i])))]),1,4)
    
    label<-paste0("\nBreakpoint: ",years[1],"[",years[2],",",years[3],"]") #need to convert to the index of these rainfall totals
    label<-paste0(label,"\nChange in slope:",round(o.seg$coefficients[2],3))
    label<-paste0(label,"\np=",pvaltext)
  }
  x=max(dati$x)*0.5
  y=max(dati$y)*0.85
  graphics::text(x,y,label)
  
  
  #residuals from double mass
  point<-which(abs(out.lm$residual)==max(abs(out.lm$residuals)))
  plot(zoo::index(s1),out.lm$residuals,xlab="year",
       ylab="Cumulative rainfall residuals (mm)")
  graphics::title(main="Residuals of double mass to straight line",cex.main=1)
  x=ifelse(zoo::index(s1)[point]+5*365>max(zoo::index(s1)),zoo::index(s1)[point]-5*365,zoo::index(s1)[point]+5*365)
  graphics::text(x,out.lm$residuals[point],substr(zoo::index(s1)[point],1,4))
  
  if(!is.na(folder)) grDevices::graphics.off()
  print(paste("Station",stationname)) #this adds a caption and space between plots when using SILOReport(), so the first figure stays with the heading
}
}

