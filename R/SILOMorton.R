#' Plot the quality codes of the input data for Morton's Evap calculations
#'
#'In development.
#'
#' @param SILO a list of sites with SILO data, as created by SILOLoad()
#' @param filename optional, filename to write a plot of the rainfall quality codes to, including extension. Filename can include full path or sub folders.
#'
#' @return a ggplot geom_tile plot of the rainfall quality codes
#'
#' @examples X<-SILOLoad(c("24001","24002","24003"))
#' @examples p<-SILOQualityCodes(X,"QualityCodes.png")
#' 
#' @export

SILOMortonQualityCodes<-function(SILO,filename=NULL)
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
    
    temp<-zoo::fortify.zoo(SILO[[i]]$tsd)
    temp<-temp %>% dplyr::select(Date=.data$Index,Tmax=.data$Smx,TMin=.data$Smn,Radn=.data$Ssl,VP=.data$Svp,Evap=.data$Sev) %>% 
      dplyr::mutate(Station=SILO[[i]]$Station,
                    Site=SILO[[i]]$Site)
    my.data<-rbind(my.data,temp)
  }
  
  my.data<-my.data %>% tidyr::gather("Variable","Code",-.data$Date,-.data$Station,-.data$Site) %>% 
    dplyr::mutate(ID=paste(.data$Station,.data$Variable,sep="-"))

  
  #Add the interpretation for each quality code
  my.data<-my.data %>% dplyr::left_join(.data$lookup,by="Code")
  
  #fix the factor order so the are in order from best to worst, not alphabetical
  my.data$Quality<-forcats::fct_relevel(my.data$Quality,as.character(lookup$Quality))
  
  #generate the plot
  p<-ggplot2::ggplot(my.data)+
    ggplot2::geom_tile(ggplot2::aes(x=.data$Date, y=factor(.data$ID),fill = factor(.data$Quality)))+
    ggplot2::scale_fill_manual(values = cols, name='Quality Code' )+
    ggplot2::theme_bw()+
    ggplot2::ylab("Station-Varible")+
    ggplot2::xlab("Date")+
    ggplot2::theme(legend.position = "top") + 
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = length(unique(my.data$Code)))) 
  
  if(!is.null(filename))  ggplot2::ggsave(filename,p,width=15,height=15,units="cm")
  return(p)
  
}

# site<-"023894"
# Radn<-bomrang::get_historical(site,type="solar")
# Tmax<-bomrang::get_historical(site,type="max")
# Tmin<-bomrang::get_historical(site,type="min")
# Rain<-bomrang::get_historical(site,type="rain")
