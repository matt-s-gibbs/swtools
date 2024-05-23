pkg.env <- new.env(parent = emptyenv())
pkg.env$cols <-c("#124734",
                 "#38A28F",
                 "#C33B32",
                 "#E27E44",
                 "#45494A",
                 "#2999A3",
                 "#031D44",
                 "#92AFD7",
                 "#4C1E4F",
                 "#B6174B",
                 "#53DD6C",
                 "#364156")

#' Download SILO data
#'
#' @param SiteList A station number or vector of station numbers, as a string (e.g. "24001")
#' @param username SILO user name. Defaults to credentials used by https://www.longpaddock.qld.gov.au/silo/point-data/
#' @param password SILO password
#' @param path Where to save the output. Will default to getwd() if not specified
#' @param startdate First day of data, in the format "YYYYMMDD". Will default to the first day of the record "18890101" if not specified
#' @param enddate Last day of data, in the format "YYYYMMDD". Will default to yesterday if not specified
#' @param ssl if true set ssl_cipher_list to "RC4-SHA" for file download. Seems to be necessary on some machines. default to FALSE

#' @return A file for each station will be saved to path, named station number.txt. Nothing is returned to the R environment.
#'
#' @examples 
#' \dontrun{
#' SILODownload(c("24001","24002","24003"),
#' path=tempdir(),
#' startdate="20180101",enddate="20200101")
#' }
#'
#' @export
SILODownload <- function(SiteList, username="noemail@net.com",password="gui",path = getwd(), startdate = "18890101", enddate = NULL,ssl=FALSE) {

    if(!dir.exists(path)){
        stop(paste("Path",path,"Doesn't exist"))
    }

    #if no end date provided, use yesterday
    if (is.null(enddate))
        enddate <- format(Sys.Date() - 1, "%Y%m%d")

    #loop through stations
    for (site in SiteList) {

        #build link
      siteToOpen <- paste0("https://longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=alldata&station=", site, "&start=", startdate,
                           "&finish=", enddate, "&username=", username, "&password=", password)

        #download data
      if(ssl){
        A<-httr::with_config( httr::config("ssl_cipher_list" = "RC4-SHA"),httr::GET(siteToOpen))
      }else
      {
        A<-httr::GET(siteToOpen)
      }
      A<-httr::content(A,as="text")
      
        #write to file
        cat(A, file = paste0(path, "/", site, ".txt"))
    }
}

#' Import a SILO file
#'
#' @param station Station number (e.g. "24001") to import. The function expects the file to be called "24001.txt".
#' @param path Location where the file is located. Use "/" or "\\\\" for folders. Defaults to getwd() if not specified.
#' @param startdate Start date of data to load, in format "YYYY-MM-DD". Defaults to start of the file if not provided
#' @param enddate End date of data to load, in format "YYYY-MM-DD". Defaults to end of the file if not provided
#'
#' @return a list of data from the file, with members: 
#' \describe{
#' \item{tsd}{the raw data as a daily zoo object} 
#' \item{Site}{the name of the site}
#' \item{Station}{the station number}
#' \item{Lon}{Longitude}
#' \item{Lat}{Latitude}
#' \item{start}{the first date with good quality rainfall data}
#' \item{end}{the last date with good quality rainfall data}
#' \item{goodpct}{the percentage of good quality coded rainfall data between start and end}
#'}
#' 
#' @importFrom utils read.table


SILOImport <- function(station, path, startdate, enddate) {
  
  if(!dir.exists(path)){
    stop(paste("Path",path,"Doesn't exist"))
  }
  
  filename <- paste0(path, "/", station, ".txt")
  fcon <- file(filename, "r")
  file <- readLines(fcon)
  close(fcon)
  
  name <- FALSE
  header <- 0
  i <- 1
  
  for (line in file) {
    #get elevation
    if (regexpr(" * Elevation", line, fixed = TRUE) > 0) {
        list<-strsplit(line," ")[[1]]
        elevation<-as.numeric(list[length(list)-2])
      }
    # strip out the station name and location
    if (name == FALSE) {
      if (regexpr(" * Patched", line, fixed = TRUE) > 0) {
        l <- line[i]
        list <- unlist(strsplit(line, " "))
        list <- list[list!=""]
        Station <- list[8]
        Site <- list[9:(match("Lat:",list)-1)]
        Site <- paste(Site,sep = " ",collapse = " ")
        Lat <- as.numeric(list[(match("Lat:",list))+1])
        Lon <- as.numeric(gsub("\\\"","",list[(match("Long:",list))+1]))
        
        name <- TRUE
      }
    } else if (header == 0) {
      if (regexpr("Date ", line, fixed = TRUE) > 0) {
        Heads = strsplit(line, " +")
        # can't have duplicate row names
        Heads[[1]][26] = "Ssp1"
        header <- 1
      }
    } else if (header == 1) {
      units <- strsplit(line, " +")
      break
    }
    i <- i + 1
  }
  # read in the data, and convert to a zoo time series object
  d <- read.table(filename, header = FALSE, col.names = Heads[[1]], skip = i)
  x <- as.Date(d$Date2, "%d-%m-%Y")
  tsd <- zoo::zoo(d[, 4:26], x)
  
  # extract on just the dates
  if (missing(startdate)) {
    startdate <- min(x,na.rm = TRUE)
  }else
  {
    startdate <- as.Date(startdate)
  }
  
  if (missing(enddate)) {
    enddate <- max(x,na.rm = TRUE)
  }else
  {
    enddate <- as.Date(enddate)
  }
  
  tsd <- stats::window(tsd, start = as.Date(startdate), end = as.Date(enddate))
  
  id<-which(tsd$Srn==0)
  missingdata<-100-length(id)/(as.numeric(enddate-startdate)+1)*100.0
  
  
  return(list(tsd = tsd, Site = Site, Station = Station, Lat = Lat, Lon = Lon, Elevation=elevation,
              start=startdate,end=enddate,missing=missingdata))
  
}

#' Import multiple SILO files
#'
#' @param sites a vector of Station numbers (e.g. c("24001","24002","24003")) to import. The function expects the file to be called "24001.txt".
#' @param path Location where the file is located. Use "/" or "\\\\" for folders. Defaults to getwd() if not specified.
#' @param startdate Start date of data to load, in format "YYYY-MM-DD". Defaults to start of the file if not provided
#' @param enddate End date of data to load, in format "YYYY-MM-DD". Defaults to end of the file if not provided
#'
#' @return a list of data from the file, with members: 
#' \describe{
#' \item{tsd}{the raw data as a daily zoo object} 
#' \item{Site}{the name of the site}
#' \item{Station}{the station number}
#' \item{Lon}{Longitude}
#' \item{Lat}{Latitude}
#' \item{start}{the first date with good quality rainfall data}
#' \item{end}{the last date with good quality rainfall data}
#' \item{goodpct}{the percentage of good quality coded rainfall data between start and end}
#'}
#'
#' @examples 
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' }
#' @export

SILOLoad<-function(sites,path = getwd(), startdate, enddate){
  
  Data<-list()
  
  for(i in 1:length(sites))   Data[[i]]<- SILOImport(sites[i],path,startdate,enddate)
    
  names(Data)<-sites
  return(Data)
  
}


#' Plot the quality codes of the SILO rainfall data
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to save a plot of the rainfall quality codes to, including extension (e.g. .png). Filename can include full path or sub folders.
#'
#' @return a ggplot geom_tile plot of the rainfall quality codes
#'
#' @examples
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' p<-SILOQualityCodes(X)
#' }
#' 
#' @export
  
SILOQualityCodes<-function(SILO,filename=NULL)
{
  #lookup table to relate quality code to what it means
  lookup<-data.frame(Code=c(0,23,13,15,35,25,75,26),
                     Quality=c("Station data, as supplied by Bureau",
                               "Nearby station, data from BoM",
                               "Deaccumulated using nearby station",
                               "Deaccumulated using interpolated data",
                               "interpolated from daily observations\nusing anomaly interpolation method",
                               "interpolated daily observations",
                               "interpolated long term average",
                               "Derived using data from other climate variables"))
  
  #colours to shade codes, green to red, derived from
  #rev(RColorBrewer::brewer.pal(7,"RdYlGn"))
  cols<-c("#1A9850", "#91CF60", "#D9EF8B", "#FFFFBF", "#FEE08B", "#FC8D59", "#D73027")
  cols<-c(cols,"#2b83ba") #add 8th item for span from a different colour palette
  names(cols)<-lookup$Quality
  
  #pull out the quality code column for each dataset in the list
  my.data<-NULL
  for(i in 1:length(SILO))
  {
    
    temp<-zoo::fortify.zoo(SILO[[i]]$tsd$Srn)
    temp$Station<-SILO[[i]]$Station
    temp$Site<-SILO[[i]]$Site
    my.data<-rbind(my.data,temp)
  }
  
  colnames(my.data)<-c("Index","Code","Station","Site")

  #Add the interpretation for each quality code
  my.data<-my.data %>% dplyr::left_join(lookup,by="Code")
  
  #fix the factor order so the are in order from best to worst, not alphabetical. Dont warn that some levels aren't used
  suppressWarnings(my.data$Quality<-forcats::fct_relevel(my.data$Quality,as.character(lookup$Quality)))
  
  #generate the plot
  p<-ggplot2::ggplot(my.data)+
    ggplot2::geom_tile(ggplot2::aes(x=.data$Index, y=factor(.data$Station),fill = factor(.data$Quality)))+
    ggplot2::scale_fill_manual(values = cols, name='Quality Code' )+
    ggplot2::theme_bw()+
    ggplot2::ylab("Station")+
    ggplot2::xlab("Date")+
    ggplot2::theme(legend.position = "top") + 
   # ggplot2::guides(fill = ggplot2::guide_legend(nrow = length(unique(my.data$Code))))  + 
    ggplot2::guides(fill = ggplot2::guide_legend(ncol=2))  + 
    ggplot2::theme(text = ggplot2::element_text(size = 10)) 
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=19,height=12,units="cm",compression="lzw",dpi = 1000)
  return(p)
  
}

#' Plot the cumulative deviation from the mean for each silo station on one plot
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension (e.g. .png). Filename can include full path or sub folders.
#' @param cols optional, a vector of colours to use for the plotting
#' 
#' @return a ggplot  plot of the cumulative deviation from the mean.
#'
#' @examples 
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' p<-SILOCumulativeDeviation(X)
#' }
#' 
#' @export

SILOCumulativeDeviation<-function(SILO,filename=NULL,cols=pkg.env$cols)
{
  #calculate cumulative deviation from the mean for each site
  dat<-lapply(SILO,function(x) cumsum(as.numeric(x$tsd$Rain-mean(x$tsd$Rain))))
  
  #reformat for plot
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
  colnames(dat)<-names(SILO)
  dat$date<-zoo::index(SILO[[1]]$tsd)
  dat<-reshape2::melt(dat,id.vars="date")
  
  p<-ggplot2::ggplot(dat)+
    ggplot2::geom_line(ggplot2::aes(date,.data$value,col=.data$variable))+
    ggplot2::theme_bw()+
    ggplot2::ylab("Cumulative deviation from mean (mm)")+
    ggplot2::xlab("Date")+
    ggplot2::scale_colour_manual(values=cols,name="Station")

  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
}

#' Produce a table summarising  SILO sites
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#'
#' @return a dataframe with the following columns
#' \describe{
#' \item{Site}{site name}
#' \item{Station}{station number}
#' \item{StartDate}{date of the first good quality rainfall data}
#' \item{EndDate}{date of the last good quality rainfall data}
#' \item{PctMissing}{percentage of days that do not have good quality code between StartDate and EndDate}
#' \item{AnnualRainfall}{Mean annual rainfall in mm}
#' \item{Latitute}{Latitude}
#' \item{Longitude}{Longitude}
#' \item{Elevation}{Elevation}
#' }
#' 
#' @examples
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' d<-SILOSiteSummary(X)
#' }
#' 
#' @export

SILOSiteSummary<-function (SILO) 
{
  X <- data.frame(Site = sapply(SILO, function(x) x$Site), 
                  Station = sapply(SILO, function(x) x$Station), 
                  StartDate = as.Date(sapply(SILO,function(x) min(zoo::index(x$tsd)[x$tsd$Srn==0])),origin="1970-01-01"), 
                  EndDate = as.Date(sapply(SILO, function(x) max(zoo::index(x$tsd)[x$tsd$Srn==0])),origin="1970-01-01"), 
                  PctMissing = round(sapply(SILO, function(x) x$missing), 
                                     digits = 2), AnnualRainfall = round(sapply(SILO, 
                                                                                function(x) mean(x$tsd$Rain) * 365.25), digits = 0), 
                  Latitude = sapply(SILO, function(x) x$Lat), Longitude = sapply(SILO, 
                                                                                 function(x) x$Lon),
                  Elevation = sapply(SILO, function(x) x$Elevation))
  return(X)
}

#' Plot a map of the SILO station locations
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#'
#' @return a google map of the SILO station locations
#'
#' @examples
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' p<-SILOMap(X)
#' }
#' 
#' @export

SILOMap<-function(SILO,filename=NULL)
{
  points<-data.frame(lon=sapply(SILO,function(x) x$Lon),
                     lat=sapply(SILO,function(x) x$Lat),
                     Station=sapply(SILO,function(x) x$Station))
  
  sp<-sf::st_as_sf(points,coords=c("lon","lat"),crs=4283)
  
  p<-ggplot2::ggplot() +
    ggspatial::annotation_map_tile(zoomin=-1, chachedir=tempdir())+
    ggspatial::layer_spatial(sp, size=5)+
    ggspatial::annotation_scale(location = "tl")+
    #ggplot2::geom_point(data = points, color = "red", size = 3) +
    ggspatial::geom_spatial_label(ggplot2::aes(.data$lon,.data$lat+0.005,label = .data$Station),points,crs = 4283)
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)

}

#' Plot double mass curves of each rainfall site against each other
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#' @param plotsperpage optional, number of plots to output per element of the list returned. Defaults to 4
#'
#' @return a list of ggplot objects that plot of the double mass curves of each station in the SILO list against each other. The double mass plot is on the bottom diagonal, and the slope of the line for each case in the upper diagonal. Each list element contains plotsperpage (default to 4) double mass plots, to allow them to be plotted on multiple pages
#'
#' @examples  
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' p<-SILODoubleMass(X)
#' }

#' @importFrom stats lm
#' @export

SILODoubleMass<-function(SILO,filename=NULL,plotsperpage=4)
{
  #munge data for ggpairs
  dat<-lapply(SILO,function(x) cumsum(as.numeric(x$tsd$Rain)))
  dat_dm<-NULL
  for(i in 1:(length(dat)-1))
  {
    for(j in (i+1):length(dat))
    {
      if(length(dat[[i]])!=length(dat[[j]])){
        stop("Data lengths need to be the same for Double Mass to work. Specify dates in SILOLoad() so a common date range is covered")
      }
      temp<-data.frame(rain1=dat[[i]],rain2=dat[[j]],site=paste0(names(dat)[i],"-",names(dat)[j]))
      dat_dm<-rbind(dat_dm,temp)
    }  
  }
  
  dat_all<-dat_dm
  dat_all$site<-as.character(dat_all$site)
  
  sites<-unique(dat_all$site)
  plots<-list()
  for(i in seq(1,length(sites),plotsperpage))
  {
    dat_dm<-dat_all[dat_all$site %in% sites[i:(i-1+plotsperpage)],]
  
    slopes<-gg_getslopes(dat_dm)
  
    p<-ggplot2::ggplot(dat_dm,ggplot2::aes(.data$rain1,.data$rain2))+
      ggplot2::geom_line()+
      ggplot2::geom_smooth(method="lm",se=FALSE,lty="dashed")+
      ggplot2::facet_wrap(~site,ncol=2)+
      ggplot2::geom_text(data=slopes,ggplot2::aes(.data$x,.data$y,label=signif(.data$slope,3)))+
      ggplot2::theme_bw()+
      ggplot2::xlab("Station Number 1")+
      ggplot2::ylab("Station Number 2")
  
    if(!is.null(filename))  ggplot2::ggsave(paste0(i,"_",filename),p,width=15,height=15,units="cm")
    plots[[floor(i/plotsperpage)+1]]<-p
  }
  return(plots)
}

#internal use, calculate slope for double mass plots
gg_getslopes<-function(dat_dm)
{
  dat_dm$site<-as.character(dat_dm$site)
  sites<-unique(dat_dm$site)
  slopes<-NULL
  for(site in sites)
  {
    dat<-dat_dm[dat_dm$site==site,]
    s<-lm(rain2~rain1+0,data=dat)$coefficients[1]
    temp<-data.frame(slope=s,site=site,x=dat$rain1[nrow(dat)*0.25],y=dat$rain2[nrow(dat)*0.75])
    slopes<-rbind(slopes,temp)
  }
  return(slopes)
}

#' Write SILO data report to word document. The report includes output from SILOSiteSummary(), SILOQualityCodes(), 
#' SILOMortonQualityCodes(). SILOMap(), SILOMonthlyRainfall(), SILOCumulativeDeviation() and SILODoubleMass().
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename filename to write the report to.
#' @param path Optional. Folder to save the report to, defaults to current working directory
#' @param cols Optional. vector of colours to use for the monthly rainfall and cumulative deviation plots. Must be at least as long as the number of sites in the SILO list.
#'
#' @return Nothing to the environment. A word document report is written to "filename".
#' 
#' @examples 
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' SILOReport(X,"MyReport.docx") #requires pandoc installed
#' }
#' 
#' @export

SILOReport<-function(SILO,filename,path=getwd(),cols=pkg.env$cols)
{
  SILO<-SILO
  cols<-cols
  if(length(SILO)>length(cols))
  {
    stop("You need to specify more colours for the number of sites, use the cols= argument")
  }
  file<-system.file("SILOReport.Rmd", package="SWTools")
  rmarkdown::render(file,output_file = filename,output_dir=path)
}
#' Plot a boxplot of monthly rainfall with mean monthly evaporation
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param evapcol name of an evaporation column to plot, defaults to "MWet".
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#' @param cols optional, a vector of colours to use for the plotting
#'
#' @return a ggplot of the monthly rainfall and evaporation.
#'
#' @examples  
#' \dontrun{
#' X<-SILOLoad(c("24001","24002","24003"),path="./SWTools/extdata")
#' p<-SILOMonthlyRainfall(X,"Span",cols=c("black","red","#124734"))
#' }
#' 
#' @export

SILOMonthlyRainfall<-function(SILO,evapcol="Mwet",filename=NULL,cols=pkg.env$cols)
{

dat<-lapply(SILO,function(x) x$tsd$Rain)
dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
colnames(dat)<-names(SILO)
dat<-zoo::zoo(dat,zoo::index(SILO[[1]]$tsd))
dat<-hydroTSM::daily2monthly(dat,FUN=sum)
dat<-zoo::fortify.zoo(dat,melt=TRUE)
dat$month<-format(dat$Index,"%b")
dat$month<-forcats::fct_relevel(dat$month,month.abb)

evap<-lapply(SILO,function(x) x$tsd[,evapcol])
evap<-data.frame(matrix(unlist(evap),nrow=length(evap[[1]]),byrow=FALSE))
colnames(evap)<-names(SILO)
evap<-zoo::zoo(evap,zoo::index(SILO[[1]]$tsd))
evap<-hydroTSM::daily2monthly(evap,sum)
evap<-hydroTSM::monthlyfunction(evap, FUN=mean, na.rm=TRUE)
evap<-t(evap)
evap<-zoo::fortify.zoo(evap,melt=TRUE)
evap$month<-month.abb[evap$Index]
evap$month<-forcats::fct_relevel(evap$month,month.abb)

p<-ggplot2::ggplot()+
  ggplot2::geom_boxplot(data=dat,ggplot2::aes(.data$month,.data$Value,fill=.data$Series), coef = 500)+
  ggplot2::geom_line(data=evap,ggplot2::aes(.data$Index,.data$Value,group=.data$Series,colour=.data$Series))+
  ggplot2::xlab("Month")+
  ggplot2::ylab("Monthly total (mm)")+
  ggplot2::theme_bw()+
  ggplot2::scale_colour_manual(values=cols,name="Station")+
  ggplot2::scale_fill_manual(values=cols,name="Station")
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
  
}

