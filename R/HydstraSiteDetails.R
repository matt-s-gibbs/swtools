#' Get streamflow station information from Hydstra API
#'
#' @param site station number, e.g. "425018"
#' @param state relevant state database for the station, e.g. "NSW"
#' @param out_folder path to folder to save outputs
#' @param flood_level optional, water level in stage datum to plot on the cross section data
#' 
#' @description
#'    
#' This function use the API associated with states' Hydstra databases to return useful site information. 
#' 
#' @details
#' 
#' Currently, the relevant websites are useful for site discovery:
#' \itemize{
#'  \item https://water-monitoring.information.qld.gov.au
#'  \item https://realtimedata.waternsw.com.au
#'  \item https://data.water.vic.gov.au/WMIS
#' }
#' 
#' The Bureau of Meteorology's \href{http://www.bom.gov.au/waterdata/}{Water Data Online} site is also useful, which can also be queried using `get_station_list()` from \href{https://github.com/buzacott/bomWater/}{BomWater package}.
#' 
#' The function will save a number of files to `out_folder` that have a file name starting with the station number followed by:
#' \itemize{
#'  \item site_info.csv: general site information returned, e.g. site name, coordinates, length of record, elevation
#'  \item x_sec.csv: chainage `chain` and elevation (`rl` in gauge datum) of the control section
#'  \item rating.csv: current rating curve, the discharge (`vf` in ML/d) for a gauge height `vf`, and also as above the cease to flow level (`above_ctf`)
#'  \item gaugings.csv: record of streamflow gaugings available
#'  \item discharge.csv: daily time series of streamflow, over a 9am - 9am period
#'  \item plot.png: summary plot of the above data
#' }
#' 
#' 
#' 
#' Quality codes shown on the plot are those used by the Bureau of Meteorology \href{http://www.bom.gov.au/water/hrs/qc_doc.shtml}{defined here}
#' 
#' 
#' @return a vector of length 3, with the number of cross sections, rating curves and streamflow gaugings found, respectively. 
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' HydstraSiteDetails("425018","NSW","c:/Temp")
#' }


HydstraSiteDetails<-function(site,state,out_folder,flood_level=NA){
  services <- tibble::tibble(states =c("QLD","NSW","VIC"),
                     URL = c("https://water-monitoring.information.qld.gov.au",
                             "https://realtimedata.waternsw.com.au",
                             "https://data.water.vic.gov.au/WMIS"))
  
  if(!(state %in% services$states)){
    stop("state must be one of QLD, NSW or VIC")
  }
  
  service <- services %>% dplyr::filter(.data$states==state) %>% dplyr::pull(URL)
  
  
  if(state == "VIC"){
    return_name <- "_return"
  }else{
    return_name <- "return"
  }
  
  if(!dir.exists(out_folder)) dir.create(paste0(out_folder))
  
  out_folder <- paste0(out_folder,"/",site,"/")
  if(!dir.exists(out_folder)) dir.create(paste0(out_folder))
  
  
  ############################
  #
  # site info
  #
  ###########################
  
  URL<-paste0(service, "/cgi/webservice.pl?%7b%22function%22:%22get_db_info%22,%22version%22:%223%22,%22params%22:%7b%22table_name%22:%22SITE%22,%22return_type%22:%22array%22,%22filter_values%22:%7b%22station%22:%22", 
              site, "%22%7d%7d%7d")
  X<-httr::GET(URL)
  X<-jsonlite::fromJSON(httr::content(X,"text", encoding = "UTF-8"))
  
  if(X$error_num!=0){
    warning(paste(site,X$error_msg))
  }
  
  if(length(X[[return_name]]$rows)<1){
    warning(paste(site,"in",state,"not found"))
    return(c(0,0,0))
  }
  
  write.csv(t(X[[return_name]]$rows),paste0(out_folder,site,"_site_info.csv"))
  
  start_date <- X[[return_name]]$rows$commence
  station_name <- stringr::str_to_title(X[[return_name]]$rows$stname)
  
  ####################
  #
  #cross section
  #
  ###################
  
  
  URL <- paste0(service,"/cgi/webservice.exe?{%22function%22:%22get_cross_sections%22,%22version%22:%221%22,%22params%22:{%22site_list%22:%22",
                site,
                #"%22,%22section_types%22:[%22XS%22,%22WR%22,%22BR%22],%22comments%22:%22yes%22,%22gauge_datum%22:%22yes%22}}")
                "%22,%22section_types%22:[%22XS%22],%22comments%22:%22yes%22,%22gauge_datum%22:%22yes%22}}")
  
  X<-httr::GET(URL)
  X<-jsonlite::fromJSON(httr::content(X,"text", encoding = "UTF-8"))
  
  if(X$error_num!=0){
    warning(paste(site,X$error_msg))
  }
  
  if(is.null(X[[return_name]]$sections)){
    warning(paste("No cross sections returned for", site, "in", state))
    no_xsec <- 0
    x_sec <- tibble::tibble(chain=NA,rl=NA)
  }else{
    
    no_xsec <- length(X[[return_name]]$sections)
    
    cs <- X[[return_name]]$sections
    
    
    #need to review if there is more than one returned
    if(prod(dim(cs))>1) print("############# more than one xsec########################")
    cs_dat <- cs[,1][[1]] #############TODO - if more than one returned, handle this better
    x_sec <- cs_dat %>% dplyr::mutate(rl=as.numeric(.data$rl),
                               chain=as.numeric(.data$chain))
    
    x_sec$commnt <- gsub("\n","",x_sec$commnt)
    x_sec$commnt <- gsub("\r","",x_sec$commnt)
    
    write.csv(x_sec,paste0(out_folder,site,"_x_sec.csv"))
  }
  
  ############################
  #
  #Rating curve
  #
  ##########################
  
  flow_var <- 141
  scale <- 1
  if(state=="QLD"){
    flow_var <- 140 #QLD ratings are in m3/s
    scale <- 86.4  #convert to ML/d
  }
  
  URL <- paste0(service, "/cgi/webservice.pl?%7b%22function%22:%22get_effective_rating%22,%22version%22:%221%22,%22params%22:%7b%22site_list%22:%22",
                site,
                "%22,%22table_from%22:%22100%22,%22table_to%22:%22",
                flow_var,
                "%22,%22interval%22:%220.1%22%7d%7d")
  X<-httr::GET(URL)
  X<-jsonlite::fromJSON(httr::content(X,"text", encoding = "UTF-8"))
  
  
  if(X$error_num!=0){
    warning(paste(site,X$error_msg))
  }
  
  
  rating <- X[[return_name]]$sites$points[[1]] ###Check there aren't older ratings in here
  
  if(is.null(rating)){
    no_rating <- 0
    rating <- tibble::tibble(vt=NA,vf=NA)
    
  }else{
    no_rating <- length(X[[return_name]]$sites$points)
    rating$vt <- as.numeric(rating$vt)
    rating$vf <- as.numeric(rating$vf)
    ctf <- as.numeric(X[[return_name]]$sites$ctf)
    rating$above_ctf <- pmax(0,rating$vf - ctf)
    rating$vt <- rating$vt * scale
    
    write.csv(rating,paste0(out_folder,site,"_rating.csv"))
  }
  
  ####################################
  #
  # gaugings
  #
  ###################################
  
  URL<-paste0(service, "/cgi/webservice.pl?%7b%22function%22:%22get_db_info%22,%22version%22:%223%22,%22params%22:%7b%22table_name%22:%22GAUGINGS%22,%22return_type%22:%22array%22,%22filter_values%22:%7b%22stn%22:%22", 
              site, "%22%7d%7d%7d")
  X<-httr::GET(URL)
  X<-jsonlite::fromJSON(httr::content(X,"text", encoding = "UTF-8"))
  
  
  if(X$error_num!=0){
    warning(paste(site,X$error_msg))
  }
  
  gaugings <- X[[return_name]]$rows
  
  if(length(gaugings)<1){
    no_gaugings <- 0
    warning(paste(site,"in",state,"has no gaugings"))
    gaugings <- NULL
  }else{
    gaugings <- gaugings %>% dplyr::mutate(m_gh = as.numeric(.data$m_gh),
                                    flow = as.numeric(.data$flow) * scale, #convert QLD to ML/d
                                    meas_date = as.Date(as.character(.data$meas_date),format="%Y%m%d"),
                                    decade = lubridate::year(lubridate::floor_date(.data$meas_date,unit="10years")))
    
    no_gaugings <- nrow(gaugings)
    
    write.csv(gaugings,paste0(out_folder,site,"_gaugings.csv"))
  }
  
  ###############################################
  #
  # time series data
  #
  ##############################################
  
  URL <- paste0(service,"/cgi/webservice.pl?%7b%22function%22:%22get_ts_traces%22,%22version%22:%222%22,%22params%22:%7b%22site_list%22:%22",
                site,
                "%22,%22datasource%22:%22A%22,%22varfrom%22:%22100.00%22,%22varto%22:%22",
                flow_var,
                ".00%22,%22start_time%22:%22",
                start_date,
                "090000%22,%22end_time%22:%22",
                format(Sys.Date(),"%Y%m%d"),
                "090000%22,%22data_type%22:%22mean%22,%22interval%22:%22day%22,%22multiplier%22:%221%22%7d%7d")
  
  #URL<-paste0(service, "/cgi/webservice.pl?%7b%22function%22:%22get_db_info%22,%22version%22:%223%22,%22params%22:%7b%22table_name%22:%22GAUGINGS%22,%22return_type%22:%22array%22,%22filter_values%22:%7b%22stn%22:%22", 
  #            site, "%22%7d%7d%7d")
  X<-httr::GET(URL)
  X<-jsonlite::fromJSON(httr::content(X,"text", encoding = "UTF-8"))
  
  
  if(X$error_num!=0){
    warning(paste(site,X$error_msg))
    dat <- NULL
  }else{
    
    dat<-X[[return_name]]$traces$trace[[1]]
    
    if(length(dat)<1){
      dat <- NULL
      warning(paste(site,"no flow data"))
      
    }else{
      dat$t<-as.POSIXct(as.character(dat$t),format="%Y%m%d%H%S",tz="UTC")+86400 #need to add a day to match rainfall, value over the previous 24 hours
      dat$v<-as.numeric(dat$v) * scale#v=discharge
      dat<-dat[,c("t","v","q")]
      names(dat)<-c("Date","Value","Qual")
      
      #trim to non missing data
      dat <- dat[min(which(dat$Qual!=255)):max(which(dat$Qual!=255)),]
      
      write.csv(dat,paste0(out_folder,site,"_discharge.csv"),row.names = FALSE,quote = FALSE)
    }
  }
  
  ##########################################
  #
  #plot
  #
  ########################################
  
  dat_line <- x_sec %>% dplyr::select(value = .data$chain, stage =.data$ rl) %>% dplyr::mutate(var = "Cross section (chainage, m)") %>% 
    dplyr::bind_rows(rating %>% dplyr::select(value=.data$vt, stage = .data$vf) %>% dplyr::mutate(var = paste0("Rating (discharge, ML/d)"))) 
  
  p1<-ggplot2::ggplot(dat_line)+ggplot2::geom_line(ggplot2::aes(.data$value,.data$stage))+
    ggplot2::facet_grid(cols=ggplot2::vars(.data$var),scales="free_x",switch = "x")+
    ggplot2::theme_bw()+
    ggplot2::labs(x=NULL,y="Gauge height (m)", colour="Gaugings")+
    ggplot2::theme(strip.placement = "outside",
          strip.background = ggplot2::element_blank())
  
  if(!is.na(flood_level)) p1 <- p1 + ggplot2::geom_hline(yintercept = flood_level,colour="pink",linetype="dashed")
  
  if(length(gaugings)>0){
    dat_points <- gaugings %>% dplyr::select(value=.data$flow,stage=.data$m_gh,decade = .data$decade)%>% 
      dplyr::mutate(var = paste0("Rating (discharge, ML/d)")) 
    p1<- p1 + ggplot2::geom_point(data=dat_points,ggplot2::aes(.data$value,.data$stage,colour=factor(.data$decade)))
  }
  
  #get consistent quality codes
  fpath <- system.file("extdata", paste0("quality_codes/", state, ".csv"), package="SWTools")
  codes <- readr::read_csv(fpath,show_col_types = FALSE) %>% 
    dplyr::select(Qual=.data$Code,code=.data$BomCode)
  
  if(length(dat)<1){
    p2 <- NULL
    p3<-NULL
  }else{
    dat_qual <- dat %>%  dplyr::left_join(codes,by="Qual")
    qual_cols <- c("#99CCFF","#CCFFCC","#FFCC99","yellow","red")
    names(qual_cols)<-c("A","B","C","E","F")
    
    p2<-ggplot2::ggplot(dat_qual)+
      ggplot2::geom_rect(ggplot2::aes(xmin=.data$Date-86400,xmax=.data$Date,ymin=0,ymax=Inf,fill=.data$code),alpha=0.33)+
      ggplot2::geom_line(ggplot2::aes(.data$Date,.data$Value))+
      ggplot2::geom_hline(yintercept = max(gaugings$flow,na.rm=TRUE),linetype="dashed",colour="darkgrey")+
      ggplot2::scale_fill_manual(values=qual_cols,name="Quality")+
      ggplot2::scale_y_log10(limits=c(1e-2,max(dat_qual$Value)))+
      ggplot2::theme_bw()+
      ggplot2::labs(x=NULL,y="Discharge (ML/d)")
    
    p3 <- ggplot2::ggplot(dat_qual)+
      ggplot2::stat_ecdf(ggplot2::aes(.data$Value,y=1- ggplot2::after_stat(.data$y)))+
      ggplot2::coord_flip()+
      ggplot2::geom_vline(xintercept = max(gaugings$flow,na.rm=TRUE),linetype="dashed",colour="darkgrey")+
      ggplot2::scale_x_log10()+
      ggplot2::labs(y="proportion of time exceeded",x="Discharge (ML/d)")+
      ggplot2::theme_bw()
  }
  
  p <- suppressWarnings(ggpubr::annotate_figure(ggpubr::ggarrange(ggpubr::ggarrange(p1,p3,nrow=1,widths = c(2,1)),p2,ncol=1),paste(site,station_name)))
  
  suppressWarnings(ggplot2::ggsave(paste0(out_folder,site,"_plot.png"),p,width=22,height=15,units="cm",dpi=600,bg="white"))
  
  return(c(no_xsec,no_rating,no_gaugings))
}
