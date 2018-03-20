#' Download SILO data
#'
#' @param SiteList A station number or vector of station numbers, as a string (e.g. "24001")
#' @param path Where to save the output. Will default to getwd() if not specified
#' @param startdate First day of data, in the format "YYYYMMDD". Will default to the first day of the record "18890101" if not specified
#' @param enddata Last day of data, in the format "YYYYMMDD". Will default to yesterday if not specified
#'
#' @return A file for each station will be saved to path, named station number.txt. Nothing is returned to the R environment.
#'
#' @examples SILODownload("24001")
#' @examples SILODownload("24001","C:/SILO/","20170701","20170801")
#'
#'
SILODownload <- function(SiteList, path = getwd(), startdate = "18890101", enddate = NULL) {

    if(!dir.exists(path)){
        print(paste("Path",path,"Doesn't exist"))
        return(-1)
    }
    username <- "SADWLBC"
    password = "25732"

    #if no end date provided, use yesterday
    if (is.null(enddate))
        enddate <- format(Sys.Date() - 1, "%Y%m%d")

    #loop through stations
    for (site in SiteList) {

        #build link
        siteToOpen <- paste0("https://www.longpaddock.qld.gov.au/cgi-bin/silo/PatchedPointDataset.php?format=alldata&station=", site, "&start=", startdate,
            "&finish=", enddate, "&username=", username, "&password=", password)

        #download data
        A <- RCurl::getURL(siteToOpen, .opts = list(ssl.verifypeer = FALSE))

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
#' tsd - the raw data as a daily zoo object, 
#' Site- the name of the site, 
#' Station - the station number,
#'Lon- Longitude,
#'Lat - Latitude,
#'start - the first date with good quality rainfall data
#'end - the last date with good quality rainfall data
#'goodpct - the percentage of good quality coded rainfall data between start and end
#'
#' @examples X<-SILOImport("24001")
#' @examples plot(X$tsd$Rain)


SILOImport <- function(station, path = getwd(), startdate, enddate) {

    if(!dir.exists(path)){
      print(paste("Path",path,"Doesn't exist"))
      return(-1)
    }

    filename <- paste0(path, "/", station, ".txt")
    fcon <- file(filename, "r")
    file <- readLines(fcon)
    close(fcon)

    name <- FALSE
    header <- 0
    i <- 1

    for (line in file) {
        # strip out the station name and location
        if (name == FALSE) {
            if (regexpr(" * Patched", line, fixed = TRUE) > 0) {
                list <- strsplit(line, ":+")
                statname <- strsplit(list[[1]][2], " ")
                Station <- statname[[1]][2]
                Site <- statname[[1]][3]

                statname <- strsplit(list[[1]][3], " ")
                Lat <- as.numeric(statname[[1]][2])

                statname <- gsub("\"", "", list[[1]][4])
                statname <- gsub(" ", "", statname)
                Lon <- as.numeric(statname)

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
        startdate <- start(tsd)
    }

    if (missing(enddate)) {
        enddate <- end(tsd)
    }

    tsd <- window(tsd, start = as.Date(startdate), end = as.Date(enddate))
    
    id<-which(tsd$Srn==0)
    startdata<-zoo::index(tsd[id[1],])
    enddata<-zoo::index(tsd[id[length(id)],])
    missingdata<-100-length(id)/(as.numeric(enddata-startdata)+1)*100.0


    return(list(tsd = tsd, Site = Site, Station = Station, Lat = Lat, Lon = Lon,
                start=startdata,end=enddata,missing=missingdata))

}

#' Import multiple SILO files
#'
#' @param sites a vector of Station numbers (e.g. c("24001","24002","24003") to import. The function expects the file to be called "24001.txt".
#' @param path Location where the file is located. Use "/" or "\\\\" for folders. Defaults to getwd() if not specified.
#' @param startdate Start date of data to load, in format "YYYY-MM-DD". Defaults to start of the file if not provided
#' @param enddate End date of data to load, in format "YYYY-MM-DD". Defaults to end of the file if not provided
#'
#' @return a list of lists of SILO data from each file. Each list has members: tsd - the raw data as a daily zoo object, Site- the name of the site, Station - the station number, Lon- Longitude, and Lat - Latitude
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples plot(X$tsd$Rain)

SILOLoad<-function(sites,path = getwd(), startdate, enddate)
{
  Data<-list()
  for(s in sites) Data[[s]]<-SILOImport(s,path,startdate,enddate)
  return(Data)
}

#' Plot the quality codes of the SILO rainfall data
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write a plot of the rainfall quality codes to, including extension. Filename can include full path or sub folders.
#'
#' @return a ggplot geom_tile plot of the rainfall quality codes
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILOQualityCodes(X,"QualityCodes.png")
  
SILOQualityCodes<-function(SILO,filename=NULL)
{
  #lookup table to relate quality code to what it means
  lookup<-data.frame(Code=c(0,23,13,15,35,25,75,26),
                     Quality=c("Station data, as supplied by Bureau",
                               "Nearby station, data from BoM",
                               "Deaccumulated using nearby station",
                               "Deaccumulated using interpolated data",
                               "interpolated from daily observations using anomaly interpolation method",
                               "interpolated daily observations",
                               "interpolated long term average",
                               "synthetic pan evaporation"))
  
  #colours to shade codes, green to red
  cols<-rev(RColorBrewer::brewer.pal(7,"RdYlGn"))
  cols<-c(cols,cols[1]) #add 8th item for span
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
  
  #fix the factor order so the are in order from best to worst, not alphabetical
  my.data$Quality<-forcats::fct_relevel(my.data$Quality,as.character(lookup$Quality))
  
  #generate the plot
  p<-ggplot2::ggplot(my.data)+
    ggplot2::geom_tile(ggplot2::aes(x=Index, y=factor(Station),fill = factor(Quality)))+
    ggplot2::scale_fill_manual(values = cols, name='Quality Code' )+
    ggplot2::theme_bw()+
    ggplot2::ylab("Station")+
    ggplot2::xlab("Date")+
    ggplot2::theme(legend.position = "top") + 
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = length(unique(my.data$Code)))) 
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
  
}

#' Plot double mass curves of each rainfall site against each other
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#'
#' @return a ggplot  plot of the double mass curves of each station in the SILO list against each other. The double mass plot is on the bottom diagonal, and the slope of the line for each case in the upper diagonal. 
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILODoubleMass(X,"DoubleMass.png")

SILODoubleMass<-function(SILO,filename=NULL)
{
  #munge data for ggpairs
  dat<-lapply(SILO,function(x) cumsum(as.numeric(x$tsd$Rain)))
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))

  colnames(dat)<-paste0("S",names(SILO))
  
  p<-GGally::ggpairs(dat,upper=list(continuous=gg_slope),diag="blankDiag",lower=list(continuous=gg_doublemass))
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
}

#' Plot the cumulative deviation from the mean for each silo station on one plot
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#'
#' @return a ggplot  plot of the cumulative deviation from the mean.
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILOCumulativeDeviation(X,"Cumulative.png")

SILOCumulativeDeviation<-function(SILO,filename=NULL)
{
  #calculate cumulative deviation from the mean for each site
  dat<-lapply(SILO,function(x) cumsum(as.numeric(x$tsd$Rain-mean(x$tsd$Rain))))
  
  #reformat for plot
  dat<-data.frame(matrix(unlist(dat),nrow=length(dat[[1]]),byrow=FALSE))
  colnames(dat)<-names(SILO)
  dat$date<-zoo::index(SILO[[1]]$tsd)
  dat<-reshape2::melt(dat,id.vars="date")
  
  p<-ggplot2::ggplot(dat)+
    ggplot2::geom_line(ggplot2::aes(date,value,col=variable))+
    ggplot2::theme_bw()+
    ggplot2::ylab("Cumulative deviation from mean (mm)")+
    ggplot2::xlab("Date")+
    ggplot2::scale_colour_discrete(name="Station")

  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
}

#' Produce a table summarising  SILO sites
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#'
#' @return a dataframe with the following columns
#' Site - site name
#' Station - station number
#' StartDate - date of the first good quality rainfall data
#' EndDate - date of the last good quality rainfall data
#' PctMissing - percentage of days that do not have good quality code between StartDate and EndDate
#' AnnualRainfall - Mean annual rainfall in mm
#' Latitute - Latitude
#' Longitude - Longitude
#' 
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples d<-SILOSummary(X)

SILOSiteSummary<-function(SILO)
{
  X<-data.frame(Site=sapply(SILO,function(x) x$Site),
                Station=sapply(SILO,function(x) x$Station),
                StartDate=as.Date(sapply(SILO,function(x) x$start),origin="1970-01-01"),
                EndDate=as.Date(sapply(SILO,function(x) x$end),origin="1970-01-01"),
                PctMissing=round(sapply(SILO,function(x) x$missing),digits = 2),
                AnnualRainfall=round(sapply(SILO,function(x) mean(x$tsd$Rain)*365.25),digits = 0),
                Latitude=sapply(SILO,function(x) x$Lat),
                Longitude=sapply(SILO,function(x) x$Lon))
  
  return(X)
}

#' Plot a map of the SILO station locations
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write the plot to, including extension. Filename can include full path or sub folders.
#'
#' @return a google map of the SILO station locations
#'
#' @examples X<-LoadSILO(c("24001","24002","24003")
#' @examples p<-SILOMap(X,"Locations.png")

SILOMap<-function(SILO,filename=NULL)
{
  points<-data.frame(lon=sapply(SILO,function(x) x$Lon),
                     lat=sapply(SILO,function(x) x$Lat),
                     Station=sapply(SILO,function(x) x$Station))
  
  sbbox <- ggmap::make_bbox(lon = points$lon, lat = points$lat, f = 0.25)
  
  sq_map <- ggmap::get_map(location = sbbox,  maptype = "terrain", source = "google")
  
  p<-ggmap::ggmap(sq_map) + 
    ggplot2::geom_point(data = points, color = "red", size = 3) +
    ggplot2::geom_text(data = points, ggplot2::aes(label = Station), hjust = 0,nudge_x = 0.005)#, color = "yellow")
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)

}

#For internal use, subplots for SILODoubleMass
gg_doublemass <- function(data, mapping, ...) {
  ggplot2::ggplot(data = data, mapping = mapping) +
    ggplot2::geom_line(color = I("blue")) + 
    ggplot2::geom_smooth(method = "lm", color = I("black"), ...)+
    ggplot2::theme_bw()
}

#For internal use, largely taken from cor in ggally, changing correlation to slope.
gg_slope<-function (data, mapping, alignPercent = 0.6, method = "pearson", 
                 use = "complete.obs", corAlignPercent = NULL, corMethod = NULL, 
                 corUse = NULL, ...) 
{
  if (!is.null(corAlignPercent)) {
    stop("'corAlignPercent' is deprecated.  Please use argument 'alignPercent'")
  }
  if (!is.null(corMethod)) {
    stop("'corMethod' is deprecated.  Please use argument 'method'")
  }
  if (!is.null(corUse)) {
    stop("'corUse' is deprecated.  Please use argument 'use'")
  }
  useOptions <- c("all.obs", "complete.obs", "pairwise.complete.obs", 
                  "everything", "na.or.complete")
  use <- pmatch(use, useOptions)
  if (is.na(use)) {
    warning("correlation 'use' not found.  Using default value of 'all.obs'")
    use <- useOptions[1]
  }
  else {
    use <- useOptions[use]
  }
  cor_fn <- function(x, y) {
    as.numeric(lm(y~x+0)$coefficients[1])
  }
  xCol <- deparse(mapping$x)
  yCol <- deparse(mapping$y)
  
  # if (is.numeric(eval_data_col(data, mapping$colour))) {
  #   stop("ggally_cor: mapping color column must be categorical, not numeric")
  # }
  colorCol <- deparse(mapping$colour)
  singleColorCol <- ifelse(is.null(colorCol), NULL, paste(colorCol, 
                                                          collapse = ""))
  if (use %in% c("complete.obs", "pairwise.complete.obs", "na.or.complete")) {
    if (length(colorCol) > 0) {
      if (singleColorCol %in% colnames(data)) {
        rows <- complete.cases(data[c(xCol, yCol, colorCol)])
      }
      else {
        rows <- complete.cases(data[c(xCol, yCol)])
      }
    }
    else {
      rows <- complete.cases(data[c(xCol, yCol)])
    }
    if (any(!rows)) {
      total <- sum(!rows)
      if (total > 1) {
        warning("Removed ", total, " rows containing missing values")
      }
      else if (total == 1) {
        warning("Removing 1 row that contained a missing value")
      }
    }
    data <- data[rows, ]
  }
  xVal <- data[[xCol]]
  yVal <- data[[yCol]]
  if (length(names(mapping)) > 0) {
    for (i in length(names(mapping)):1) {
      tmp_map_val <- deparse(mapping[names(mapping)[i]][[1]])
      if (tmp_map_val[length(tmp_map_val)] %in% colnames(data)) 
        mapping[[names(mapping)[i]]] <- NULL
      if (length(names(mapping)) < 1) {
        mapping <- NULL
        break
      }
    }
  }
  if (length(colorCol) < 1) {
    colorCol <- "ggally_NO_EXIST"
  }
  if ((singleColorCol != "ggally_NO_EXIST") && (singleColorCol %in% 
                                                colnames(data))) {
    cord <- ddply(data, c(colorCol), function(x) {
      cor_fn(x[[xCol]], x[[yCol]])
    })
    colnames(cord)[2] <- "ggally_cor"
    cord$ggally_cor <- signif(as.numeric(cord$ggally_cor), 
                              2)
    lev <- levels(data[[colorCol]])
    ord <- rep(-1, nrow(cord))
    for (i in 1:nrow(cord)) {
      for (j in seq_along(lev)) {
        if (identical(as.character(cord[i, colorCol]), 
                      as.character(lev[j]))) {
          ord[i] <- j
        }
      }
    }
    cord <- cord[order(ord[ord >= 0]), ]
    cord$label <- str_c(cord[[colorCol]], ": ", cord$ggally_cor)
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * 
                  (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * 
                  (ymax - ymin))
    p <- GGally::ggally_text(label = str_c("", signif(cor_fn(xVal, 
                                                           yVal), 2)), mapping = mapping, xP = 0.5, yP = 0.9, 
                     xrange = xrange, yrange = yrange, color = "black", 
                     ...) + ggplot2::theme(legend.position = "none")+ 
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    xPos <- rep(alignPercent, nrow(cord)) * diff(xrange) + 
      min(xrange, na.rm = TRUE)
    yPos <- seq(from = 0.9, to = 0.2, length.out = nrow(cord) + 
                  1)
    yPos <- yPos * diff(yrange) + min(yrange, na.rm = TRUE)
    yPos <- yPos[-1]
    cordf <- data.frame(xPos = xPos, yPos = yPos, labelp = cord$label)
    cordf$labelp <- factor(cordf$labelp, levels = cordf$labelp)
    p <- p + GGally::geom_text(data = cordf, aes(x = xPos, y = yPos, 
                                         label = labelp, color = labelp), hjust = 1, ...)+
      ggplot2::theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
    p
  }
  else {
    xmin <- min(xVal, na.rm = TRUE)
    xmax <- max(xVal, na.rm = TRUE)
    xrange <- c(xmin - 0.01 * (xmax - xmin), xmax + 0.01 * 
                  (xmax - xmin))
    ymin <- min(yVal, na.rm = TRUE)
    ymax <- max(yVal, na.rm = TRUE)
    yrange <- c(ymin - 0.01 * (ymax - ymin), ymax + 0.01 * 
                  (ymax - ymin))
    p <- GGally::ggally_text(label = paste("", signif(cor_fn(xVal, 
                                                             yVal), 2), sep = "", collapse = ""), mapping, xP = 0.5, 
                     yP = 0.5, xrange = xrange, yrange = yrange, ...) + 
      ggplot2::theme(legend.position = "none")+ 
      ggplot2::theme(panel.grid.major = ggplot2::element_blank(), panel.grid.minor = ggplot2::element_blank())
    p
  }
}
